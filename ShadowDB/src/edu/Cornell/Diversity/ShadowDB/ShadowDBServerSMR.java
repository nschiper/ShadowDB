/* Copyright 2012-2014 Cornell University
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
 *  o Date:        1st March 2014
 *  o File name:   ShadowDBServerSMR.java
 *  o Description: The shadowDB server when replicated with state machine replication (SMR).
 */

package edu.Cornell.Diversity.ShadowDB;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Logger;

import edu.Cornell.Diversity.ResilientTCP.FailFastSocket;
import edu.Cornell.Diversity.TOBroadcast.AnerisMessage;
import edu.Cornell.Diversity.TOBroadcast.AnerisMessage.ProtocolType;
import edu.Cornell.Diversity.TOBroadcast.TobcastClient;
import edu.Cornell.Diversity.TOBroadcast.TobcastClient.AnerisType;
import edu.Cornell.Diversity.Utils.ConfigurationParser;
import edu.Cornell.Diversity.Utils.DbUtils;
import edu.Cornell.Diversity.Utils.TransactionIdGenerator;
import edu.Cornell.Diversity.Utils.DbUtils.DB_TYPE;
import edu.Cornell.Diversity.Utils.IdIpPort;
import edu.Cornell.Diversity.Utils.ShutdownHook;

/**
 * This class implements a database replica. The replication
 * protocol is based on state machine replication: each transaction
 * is totally ordered using Aneris and executed on the database upon
 * delivery.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class ShadowDBServerSMR extends Thread implements ShadowDB {

	private static final Logger LOG = Logger.getLogger("edu.Cornell.Diversity.ShadowDB.ShadowDBServer");

	private static final ConcurrentHashMap<String, QueryResult> TRANS_EXECUTED =
		new ConcurrentHashMap<String, QueryResult>(100 /* initial capacity */);

	private TobcastClient tobcastClient;

	/**
	 * A client handler per client id.
	 */
	private Hashtable<String, ClientHandler> clientHandlers;

	private ServerSocketChannel serverSocket;

	/**
	 * Selection keys used for asynchronous I/O.
	 */
	private SelectionKey serverSocketKey;

	private Selector selector;

	private String dbId;

	private Connection conn;

	private TransactionIdGenerator idGen;

	private ShadowDBServerSMR(String dbId, DB_TYPE dbType, ServerSocketChannel serverSocket,
		TobcastClient tobcastClient, Selector selector) throws Exception {

	    this.dbId = dbId;
	    this.serverSocket = serverSocket;
	    this.tobcastClient = tobcastClient;
	    this.selector = selector;

		if (dbId.equals("database1")) {
			tobcastClient.toBcast(new AnerisMessage(ProtocolType.PAXOS));
		}

	    this.clientHandlers = new Hashtable<String, ClientHandler>(100 /* initial capacity */);

    	this.conn = DbUtils.openDbSql(dbType);
    	this.idGen = new TransactionIdGenerator();
	}



	public void closeDb() throws SQLException {
        if (conn != null) {
            conn.close();
        }
	}

	public String getDbId() {
		return this.dbId;
	}

	public String dbIdToString() {
		return "db " + dbId;
	}


	private void registerSockets() throws Exception {
		serverSocketKey = serverSocket.register(selector, SelectionKey.OP_ACCEPT);
	}

	private void onClientConnection() throws Exception {
		SocketChannel socket = serverSocket.accept();
		FailFastSocket clientSocket =
			FailFastSocket.newInstance(socket, dbId, null);

		LOG.info(dbIdToString() + " received connection from client: " + clientSocket.getEndPointId());
		ClientHandler handler = new ClientHandler(clientSocket);
		clientHandlers.put(clientSocket.getEndPointId(), handler);
		handler.start();
	}

	private void onIncomingTransaction() throws Exception {
		LinkedList<AnerisMessage> msgs = tobcastClient.deliver();

		for (AnerisMessage delivered: msgs) {
			if (delivered.getType() == AnerisMessage.MessageType.TRANS) {
				QueryResult result = delivered.execute(conn, idGen.getNextId());

				// Send the answer to the client
				String clientId = delivered.getClientId();
				TRANS_EXECUTED.put(clientId, result);
				ClientHandler handler = clientHandlers.get(clientId);
	
				synchronized(handler) {
					handler.notify();
				}
			} else {
				LOG.warning(dbIdToString() + " delivered message of type: " + delivered.getType());
			}
		}

		conn.commit();
	}

	public void run() {
		/**
		 * Registering the channels to do asynchronous I/O.
		 */
		try {
			registerSockets();
		} catch (Exception e) {
			LOG.severe(dbIdToString() + " unable to init asynchronous io, caught exception: " + e);
			return;
		}

		for (;;) {

			try {
				int nbChannelsReady = selector.select();

				if (nbChannelsReady > 0) {
					Iterator<SelectionKey> it = selector.selectedKeys().iterator();

					while (it.hasNext()) {
						SelectionKey key = it.next();
						it.remove();

						/**
						 * Incoming client connections.
						 */
						if (serverSocketKey.equals(key)) {
							onClientConnection();
						}
					}
				} else {
					/**
					 * Handling incoming transactions.
					 */
					onIncomingTransaction();
				}
			} catch (Exception e) {
				e.printStackTrace();
				LOG.warning(dbIdToString() + " caught exception: " + e);
			}
		}
	}

	private static ServerSocketChannel openServerSocket(int port) throws IOException {
		ServerSocketChannel serverSocket = ServerSocketChannel.open();
		InetSocketAddress address = new InetSocketAddress(port);

		/**
		 * This reuses the socket even if it has not closed yet.
		 * Only to be used when the other end stopped sending packets
		 * when we reopen the socket.
		 */
		serverSocket.socket().setReuseAddress(true);
		serverSocket.socket().bind(address);
		serverSocket.configureBlocking(false);
		ShutdownHook.addSocketToClose(serverSocket.socket());
		return serverSocket;
	}

	public static void main(String[] args) {
		if (args.length == 2) {
			ShadowDBServerSMR dbServer = null;

			String dbId = args[0];
			String configFile = args[1];

			try {
				ConfigurationParser configParser = new ConfigurationParser(configFile);
				LinkedList<IdIpPort> servers = configParser.getToBCastServers();
				int clientPort = configParser.getClientPort(dbId);
				int dbPort = configParser.getPortFromId(dbId);
				DB_TYPE dbType = configParser.getDbType(dbId);
				configParser.closeConfigFile();

				Selector selector = Selector.open();
				TobcastClient tobcastClient = TobcastClient.newInstance(servers, clientPort,
					AnerisType.LISP, selector);
				ServerSocketChannel serverSocket = openServerSocket(dbPort);
				dbServer = new ShadowDBServerSMR(dbId, dbType, serverSocket, tobcastClient, selector);

				ShutdownHook.addDbToClose(dbServer);
				ShutdownHook.installHook();

				dbServer.start();
				dbServer.join();
			} catch(Exception e) {
				LOG.severe("Unable to start shadowDbSMR properly: " + e);
				return;
			}
			
		} else {
			System.err.println("Please specify the db id and the configuration file");
		}
	}

	/**
	 * This inner class is responsible for accepting incoming connections from clients
	 * and sending them query results.
	 * 
	 * @author nschiper@cs.cornell.edu
	 *
	 */
	public static class ClientHandler extends Thread {

		private FailFastSocket clientSocket;
		protected AtomicBoolean finish;

		public ClientHandler(FailFastSocket clientSocket) throws IOException {
			this.clientSocket = clientSocket;
			this.finish = new AtomicBoolean(false);
		}

		public void run() {

			String clientId = clientSocket.getEndPointId();

			for (;;) {
				try {
					QueryResult result;

					synchronized(this) {
						while ((result = TRANS_EXECUTED.remove(clientId)) == null) {
							wait();
						}
					}

					clientSocket.writeObject(result);
				} catch (Exception e) {
					/**
					 *  Close the
					 *  connection and stop this thread.
					 */
					try {
						clientSocket.close();
						LOG.info("Closed connection to client: " + clientId);
						return;
					} catch (IOException ioe) {
						// There's not much to do here...
					}
				}
			}
		}
	}
}
