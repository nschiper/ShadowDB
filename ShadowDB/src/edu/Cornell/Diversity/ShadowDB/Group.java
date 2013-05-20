/* Copyright 2012 Cornell University
 * Copyright 2013 Cornell University
 *
 *
 * This file is part of ShadowDB - a replicated database based on
 * an formally-verified implementation of an atomic broadcast service.
 * The normal case processing is written in Java, while failure-handling is taken
 * care of by an atomic broadcast service called Aneris. Aneris contains
 * two formally-verified implementations of atomic broadcast, one is
 * based on Paxos, the other one is based on two-third consensus.
 * 
 * The three code contributors of this project are:
 * Vincent Rahli (Aneris)
 * Nicolas Schiper (ShadowDB)
 * Mark Bickford (Aneris)
 * 
 * ShadowDB was implemented at Cornell University, Ithaca, NY.
 *
 * ShadowDB is a free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ShadowDB is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ShadowDB.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  o Authors:     Nicolas Schiper
 *  o Affiliation: Cornell University
 *  o Date:        13 May 2013
 *  o File name:   Group.java
 *  o Description: The group of replicas of ShadowDB.
 */

package edu.Cornell.Diversity.ShadowDB;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.logging.Logger;

import edu.Cornell.Diversity.ResilientTCP.FailFastSocket;
import edu.Cornell.Diversity.ResilientTCP.SuspectedCrashException;
import edu.Cornell.Diversity.TOBroadcast.AnerisMessage;
import edu.Cornell.Diversity.TOBroadcast.TobcastClient;
import edu.Cornell.Diversity.Utils.DbUtils;
import edu.Cornell.Diversity.Utils.IdIpPort;
import edu.Cornell.Diversity.Utils.ShutdownHook;

/**
 * A class that represents a group of replicas. Each incarnation of
 * a group has primary replica and backups. This class includes methods
 * for the primary to broadcast a message to the backups, and for backups to
 * send message to the primary.
 * 
 * This class maintains the composition of the group, replacing
 * crashed replicas with fresh ones.
 * To ensure that group members observe the same sequence of group
 * incarnations, adding and removing members is done using Aneris, the total
 * order broadcast service.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class Group extends Thread {

	private static final Logger LOG = Logger.getLogger("edu.Cornell.Diversity.ShadowDB.Group");

	/**
	 * The maximum number of simultaneous tolerated failures.
	 */
	private int f;

	/**
	 *  The incarnation number of the group. This denotes
	 *  the number of times the group was reconfigured.
	 */
	private int incarnation;

	/**
	 * The primary of the group.
	 */
	private IdIpPort primary;

	/**
	 * The group members mapped by id.
	 */
	private LinkedList<IdIpPort> members;

	private FailFastSocket socketPrimary;

	/**
	 * Sockets to communicate with each one of the group backups.
	 */
	private final HashMap<String, FailFastSocket> socketMembers;
	
	/**
	 * All the known databases. Machines from this list are added to the group
	 * in case of failures.
	 */
	private final LinkedList<IdIpPort> allDbs;

	/**
	 * Index of the next database in the list allDbs that can be used to replace a crashed one.
	 */
	private int nextDbToUse;

	/**
	 * The tobcast client to interact with Aneris.
	 */
	private TobcastClient tobcastClient;

	/**
	 * The local database server that is part of this group.
	 * This is used to call the proper method of the DB server
	 * when receiving a message.
	 */
	private final ShadowDBServer registeredDb;

	/**
	 * The server socket, used by the primary to receive messages
	 * from clients, and by the backups to receive messages from the
	 * primary.
	 */
	private final ServerSocketChannel serverSocket;

	/**
	 * Selection keys used for asynchronous I/O.
	 */
	private SelectionKey[] tobcastClientKeys;
	private SelectionKey serverSocketKey;
	private final HashSet<SelectionKey> databaseKeys;

	/**
	 * Constructs a group object given the list of DBs and tobcast servers, as well as
	 * the local dbServer, and the client port (this is used by Aneris to identify
	 * clients and is found in the configuration file).
	 */
	public Group(LinkedList<IdIpPort> dbServers, LinkedList<IdIpPort> tobcastServers,
		ShadowDBServer dbServer, int serverPort, int clientPort) throws Exception {

		this.registeredDb = dbServer;
		this.allDbs = dbServers;
		this.members = new LinkedList<IdIpPort>();

		this.f = ShadowDBConfig.getF();

		// The first group members are the first f+1 in the configuration file.
		for (int i = 0; i < f + 1; i++) {
			this.members.add(allDbs.get(i));
		}
		this.nextDbToUse = f + 1;

		this.socketMembers = new HashMap<String, FailFastSocket>();

		// The primary is the first group member
		this.primary = members.get(0);

		this.incarnation = 1;

		if (registeredDb.isReplicated()) {
			int clientId = DbUtils.extractIntFromId(dbServer.getDbId());
			this.tobcastClient = TobcastClient.newInstance(tobcastServers, clientPort, clientId);
		}

	    this.serverSocket = openServerSocket(serverPort);
	    this.databaseKeys = new HashSet<SelectionKey>();

		LOG.info(groupIdToString() + ", group initialized with members: " + members);
	}

	/**
	 * This method attempts to connect to the backup servers.
	 * If unsuccessful, a SuspectedCrashException is thrown.
	 */
	private void connectToBckups() throws SuspectedCrashException {
		assert(isPrimary());

		for (int i = 1; i < f + 1; i++) {
			IdIpPort member = members.get(i);

			if (!socketMembers.containsKey(member.getId())) {
				LOG.info(registeredDb.dbIdToString() + " connecting to " + member);
				FailFastSocket ffSocket = FailFastSocket.newInstance(member.getIp(), member.getPort(),
					registeredDb.getDbId(), member.getId());
				socketMembers.put(member.getId(), ffSocket);
				LOG.info(registeredDb.dbIdToString() + " connected to backup: " + member);
			}
		}
	}

	public boolean isPrimary() {
		return registeredDb.getDbId().equals(primary.getId());
	}

	public boolean isBackup() {
		return !primary.getId().equals(registeredDb.getDbId());
	}

	private String groupIdToString() {
		if (isPrimary()) {
			return "group at primary db: " + registeredDb.getDbId();
		} else {
			return "group at backup db: " + registeredDb.getDbId();
		}
	}

	public int getGroupSize() {
		return f + 1;
	}

	public int getIncarnation() {
		return this.incarnation;
	}

	public String getPrimaryId() {
		return primary.getId();
	}

	public void sendToPrimary(TransactionId id) throws SuspectedCrashException {

		if (socketPrimary != null) {
			socketPrimary.writeObject(id);
		} else {
			LOG.severe("Error trying to send a message to the primary, the socket is null");
		}
	}

	public Object receiveFromPrimary() throws SuspectedCrashException, InterruptedException {
		if (socketPrimary != null) {
			return socketPrimary.readObject();
		} else {
			throw new IllegalStateException("Impossible to receive a message from the primary, the socket is null");
		}
	}

	public void sendToBackups(ShadowTransaction t) throws SuspectedCrashException {
	
		for (FailFastSocket socket : socketMembers.values()) {
			socket.writeObject(t);
		}
	}

	private TobcastClient getTobcastClient() {
		return this.tobcastClient;
	}

	/**
	 * This method returns a new list of members by replacing the
	 * one that is suspected to have crashed with a fresh one.
	 */
	private LinkedList<IdIpPort> replaceCrashedMember(String id) {
		LinkedList<IdIpPort> copy = new LinkedList<IdIpPort>();

		/**
		 * Copy all members except the one that is suspected to have crashed.
		 */
		for (IdIpPort member : this.members) {
			if (!id.equals(member.getId())) {
				copy.add(new IdIpPort(member.getId(), member.getIp(), member.getPort()));
			}
		}

		/**
		 * Add the next available member not already in members.
		 */
		copy.add(allDbs.get(nextDbToUse));

		return copy;
	}

	/**
	 * Remove a member from the group identified by the id parameter
	 * by broadcasting a new group configuration message. The last sequence
	 * number to be considered for the current configuration is denoted
	 * by the seqNo parameter.
	 */
	private void removeMember(String id, long seqNo) {
		LinkedList<IdIpPort> newMembers = replaceCrashedMember(id);
		AnerisMessage newConfig = new AnerisMessage(newMembers, seqNo, registeredDb.getDbId());
		tobcastClient.toBcast(newConfig);

		LOG.info("\n" + groupIdToString() + ", broadcast new group configuration: " + newConfig + "\n");
	}

	/**
	 * This method reconfigures the group by changing the group membership, the primary,
	 * and closing connections to the previous group members.
	 */
	private void reconfigureGroup(AnerisMessage newConfig, Selector selector) {
		assert(newConfig.getType() == AnerisMessage.ANERIS_MSG_TYPE.BCAST);

		LOG.info("\n" + groupIdToString() + ", received new group configuration: " + newConfig);

		this.members = newConfig.getMembers();
		this.primary = members.get(0);

		incarnation++;

		/**
		 * Set the next db to use as the database right after the last member
		 * of the current group configuration.
		 */
		int indexLastMember = allDbs.indexOf(members.get(f));
		nextDbToUse = (indexLastMember + 1) % allDbs.size();

		/**
		 * Cancel socket registrations and close connections to the previous
		 * group members.
		 */
		deregisterKeys(databaseKeys);

		try {
			// Old primary
			if (socketPrimary != null) {
				socketPrimary.close();
				socketPrimary = null;
			}
			// Old backups
			for (FailFastSocket backupSocket : socketMembers.values()) {
				backupSocket.close();
			}
			socketMembers.clear();
		} catch (Exception e) {
			LOG.warning(groupIdToString() + " unable to close connections of previous group members: " + e);
		}

		if (isPrimary()) {
			try {
				/**
				 * Wait a little bit to make sure all backups delivered the
				 * new configuration.
				 */
				Thread.sleep(ShadowDBConfig.getGroupReconfigurationTime());

				connectToBckups();
				for (FailFastSocket backup : socketMembers.values()) {
					registerSocketAndAddKey(selector, backup, databaseKeys);
				}
			} catch (Exception e) {
				if (e instanceof SuspectedCrashException) {
					SuspectedCrashException sce = (SuspectedCrashException) e;

					/**
					 * The reconfiguration failed due to a suspected failure.
					 * Request to remove the database suspected to have crashed and
					 * return to the caller.
					 */
					removeMember(sce.getId(), registeredDb.getSeqNo());
					return;
				}
			}
		}
	}

	private void registerSockets(Selector selector) throws Exception {

		if (isPrimary()) {
			for (FailFastSocket socket : socketMembers.values()) {
				registerSocketAndAddKey(selector, socket, databaseKeys);
			}
		} else {
			if (socketPrimary != null) {
				registerSocketAndAddKey(selector, socketPrimary, databaseKeys);
			}
		}
		if (registeredDb.isReplicated() && tobcastClientKeys == null) {
			tobcastClientKeys = registerAndAttachSocket(selector, getTobcastClient().getSockets());
		}

		if (serverSocketKey == null || !serverSocketKey.channel().equals(serverSocket)) {
			serverSocketKey = serverSocket.register(selector, SelectionKey.OP_ACCEPT);
		}
	}

	private void registerSocketAndAddKey(Selector selector, FailFastSocket socket,
		HashSet<SelectionKey> keys) throws ClosedChannelException {

		SelectionKey key = socket.register(selector, SelectionKey.OP_READ);
		key.attach(socket);
		keys.add(key);
	}

	private SelectionKey[] registerAndAttachSocket(Selector selector, SocketChannel[] sockets)
		throws ClosedChannelException {

		SelectionKey[] keys = new SelectionKey[sockets.length];

		for (int i = 0; i < sockets.length; i++) {
			if (sockets[i] != null && sockets[i].isConnected() && sockets[i].isOpen()) {
				keys[i] = sockets[i].register(selector, SelectionKey.OP_READ);
				keys[i].attach(sockets[i]);
			}
		}
		return keys;
	}

	private ServerSocketChannel openServerSocket(int port) throws IOException {
		ServerSocketChannel serverSocket = ServerSocketChannel.open();
		InetSocketAddress address = new InetSocketAddress(port);

		serverSocket.socket().bind(address);
		serverSocket.configureBlocking(false);
		ShutdownHook.addSocketToClose(serverSocket.socket());
		return serverSocket;
	}

	private boolean readyToDeliverMsg(SelectionKey key) {
		for (int i = 0; i < tobcastClientKeys.length; i++) {
			if (tobcastClientKeys[i] != null && tobcastClientKeys[i].equals(key)
				&& key.isReadable()) {
				return true;
			}
		}
		return false;
	}

	private void changeSocket2Primary(FailFastSocket newPrimary) throws IOException {
		if (socketPrimary != null) {
			socketPrimary.close();
		}
		socketPrimary = newPrimary;
	}

	private void deregisterKeys(HashSet<SelectionKey> keys) {

		for (SelectionKey key : keys) {
			key.cancel();
		}
		databaseKeys.clear();		
	}

	/**
	 * This method is called after the group has been initialized to receive messages from
	 * the network and issue the corresponding callbacks.
	 */
	public void run() {
		Selector selector;

		/**
		 * Register the channels for asynchronous I/O and
		 * make the primary connect to the backups.
		 */
		try {
			selector = Selector.open();
			if (isPrimary() && registeredDb.isReplicated()) {
				connectToBckups();
			}
			registerSockets(selector);
		} catch (Exception e) {
			LOG.severe(registeredDb.dbIdToString() + " unable to start replica group, caught exception: " + e);
			return;
		}

		long now;

		/**
		 * A timeout used by backups to detect initial primary failures
		 * (The primary crashes before connecting to the backups. A timeout
		 * of 0 means no timeout.
		 */
		long timeout = 0l;

		boolean reconfiguringGroup = false;

		/**
		 * The main loop.
		 */
		for (;;) {
			/**
			 * Code executed by the primary.
			 */
			if (isPrimary()) {
				try {

					int nbChannelsReady = selector.select();
					now = System.currentTimeMillis();

					if (nbChannelsReady > 0) {
						Iterator<SelectionKey> it = selector.selectedKeys().iterator();

						while (it.hasNext()) {
							SelectionKey key = it.next();
							it.remove();

							/**
							 * Incoming client connections.
							 */
							if (serverSocketKey.equals(key)) {
								SocketChannel socket = serverSocket.accept();
								FailFastSocket clientSocket =
									FailFastSocket.newInstance(socket, registeredDb.getDbId(),
										null);
								registeredDb.onNewConnection(clientSocket, selector, isPrimary());
							}
							/**
							 * Handling acknowledgments coming from backups.
							 */
							else if (databaseKeys.contains(key) & !reconfiguringGroup) {
								FailFastSocket clientSocket = (FailFastSocket) key.attachment();
								registeredDb.onMsgFromBackup(clientSocket, now);
							}
							/**
							 * Handling group reconfigurations.
							 */
							else if (registeredDb.isReplicated() && readyToDeliverMsg(key)){
								LinkedList<AnerisMessage> msgs = tobcastClient.deliver();

								for (AnerisMessage delivered : msgs) {
									if (delivered.getType() == AnerisMessage.ANERIS_MSG_TYPE.BCAST) {
										/**
										 * Ignore reconfiguration messages that contain
										 * the current group membership.
										 */
										if (!delivered.getMembers().equals(members)) {
											reconfigureGroup(delivered, selector);										
											registeredDb.onGroupReconfiguration(delivered.getSeqNo());
											reconfiguringGroup = false;
										}
									} else {
										LOG.info("Consensus protocol changed to: " + delivered.getProtocol());
									}
								}
							}
						}
					}
					/**
					 * Executing client transactions.
					 * This only done if we are not reconfiguring the group.
					 */
					if (!reconfiguringGroup) {
						registeredDb.executeTransactionsAtPrimary(now);
					}

					registeredDb.gcTransactions(now);
				} catch (Exception e) {
					if (e instanceof SuspectedCrashException) {
						SuspectedCrashException sce = (SuspectedCrashException) e;
						LOG.info("\n Detected crash of: " + sce.getId());

						if (sce.getId().startsWith("database")) {
							removeMember(sce.getId(), registeredDb.getSeqNo());
							reconfiguringGroup = true;
						}
					} else {
						LOG.warning(registeredDb.dbIdToString() + " exception: " + e + " caught in ShadowDB server.");
					}
				}
			}
			/**
			 *  Code executed by backups.
			 */
			else {
				try {
					int nbChannelsReady = selector.select(timeout);
					now = System.currentTimeMillis();

					/**
					 * If we are waiting for a connection from the primary
					 * and the primary has not connected to
					 * us yet, we remove the primary from the group.
					 */
					if (socketPrimary == null && timeout != 0l &&
						nbChannelsReady == 0) {
						removeMember(primary.getId(), registeredDb.getSeqNo());
						reconfiguringGroup = true;
					}
					timeout = 0l;

					if (nbChannelsReady > 0) {
						Iterator<SelectionKey> it = selector.selectedKeys().iterator();
	
						while (it.hasNext()) {
							SelectionKey key = it.next();
							it.remove();

							/**
							 * Handling a connection from the primary.
							 */
							if (serverSocketKey.equals(key)) {
								SocketChannel socket = serverSocket.accept();
								FailFastSocket primarySocket = FailFastSocket.newInstance(socket, registeredDb.getDbId(),
									primary.getId());

								/**
								 * Only accept connections from the current primary.
								 */
								if (primarySocket.getEndPointId().equals(getPrimaryId())) {
									registerSocketAndAddKey(selector, primarySocket, databaseKeys);
									changeSocket2Primary(primarySocket);

								} else {
									LOG.warning(registeredDb.dbIdToString() + " rejected connection from: "
										+ primarySocket.getEndPointId());
									primarySocket.close();
								}
							/**
							 * Handling transactions coming from the primary.
							 */
							} else if (databaseKeys.contains(key) && !reconfiguringGroup) {
								FailFastSocket socket = (FailFastSocket) key.attachment();
								registeredDb.onTransactionFromPrimary(socket, now);
							/**
							 * Handling group reconfigurations.
							 */
							} else if (readyToDeliverMsg(key)){
								LinkedList<AnerisMessage> msgs = tobcastClient.deliver();

								if (msgs != null) {
									for (AnerisMessage delivered : msgs) {
										if (delivered.getType() == AnerisMessage.ANERIS_MSG_TYPE.BCAST) {
											/**
											 * Ignore reconfiguration messages that contain
											 * the current group membership.
											 */
											if (!delivered.getMembers().equals(members)) {
												reconfigureGroup(delivered, selector);		
												registeredDb.onGroupReconfiguration(delivered.getSeqNo());
												reconfiguringGroup = false;

												/**
												 * The primary connects to the backups ShadowDBConfig.getGroupReconfigurationTime()
												 * after installing the new group configuration. We therefore set the timeout
												 * to two times this value if we are not the new primary.
												 */
												if (!primary.getId().equals(registeredDb.getDbId())) {
													timeout = 2 * ShadowDBConfig.getGroupReconfigurationTime();
												}
											}
										} else {
											LOG.info("Consensus protocol changed to: " + delivered.getProtocol());
										}
									}
								}
							}
						}
					}

					/**
					 * Garbage collect transaction results.
					 */
					registeredDb.gcTransactions(now);
				} catch (Exception e) {
					if (e instanceof SuspectedCrashException) {
						SuspectedCrashException sce = (SuspectedCrashException) e;
						LOG.info("\n Detected crash of: " + sce.getId() + "\n");
						if (sce.getId().startsWith("database")) {
							removeMember(sce.getId(), registeredDb.getSeqNo());
							reconfiguringGroup = true;
						}
					}
				}
			}
		}
	}
}
