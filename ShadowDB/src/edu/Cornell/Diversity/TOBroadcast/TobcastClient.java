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
 *  o File name:   TobcastClient.java
 *  o Description: The client interfacing with the Aneris atomic broadcast service.
 */

package edu.Cornell.Diversity.TOBroadcast;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Logger;

import edu.Cornell.Diversity.Utils.IdIpPort;
import edu.Cornell.Diversity.Utils.NIOUtils;

public class TobcastClient extends Thread {

	private static final Logger LOG = Logger.getLogger("edu.Cornell.Diversity.TOBroadcast.TobcastClient");

	/**
	 * The maximum number of bytes for a message residing
	 * in a byte buffer. This can be increased if needed.
	 */
	private static final int MAX_MSG_SIZE = 50000;

	/**
	 * Message separator used when batching messages.
	 */
	public static final String MSG_SEPARATOR = "[)),]";

	/**
	 * This is a constant used to specify external tobcast client that
	 * will only braodcast messages but not deliver them. External
	 * clients are not present in Aneris's configuration file.
	 */
	public static final int EXTERNAL_CLIENT = -1;

	public static final long BATCHING_TIMEOUT = 1l;

	private final static ConcurrentHashMap<Long, LinkedList<AnerisMessage>> DELIVERED_MSG_BUFF =
		new ConcurrentHashMap<Long, LinkedList<AnerisMessage>>();

	private final static LinkedBlockingQueue<AnerisMessage> TOBCAST_MSG_BUF =
		new LinkedBlockingQueue<AnerisMessage>();

	private final static LinkedList<AtomicLong> CLIENT_SLOTS =
		new LinkedList<AtomicLong>();

	private static Lock DELIVERED_MSG_LOCK;
	private static Condition DELIVERED_MSG_COND;

	/**
	 * Starting slot of last garbage collection of DELIVERED_MSG_BUF.
	 */
	private static long GC_SLOT = 1;

	private static Selector SELECTOR;

	private static boolean THREAD_STARTED = false;

	public enum AnerisType {INTERPRETED, LISP};

	private static AnerisType ANERIS_TYPE;

	private static SocketChannel[] SOCKETS;

	private final LinkedList<IdIpPort> servers;

	/**
	 * This represents the index of the next message the client
	 * should deliver. This is used to ensure clients deliver messages
	 * without gaps. Gaps may happen because the to-bcast server
	 * we were connected to crashed and we had to reconnect to a
	 * new one, missing some messages in the meantime.
	 */
	private AtomicLong indexNextMsg;

	/**
	 * A selector on which to call wake-up when messages
	 * can be delivered. This field is not always initialized.
	 */
	private Selector registeredSelector;

	/**
	 * Creates a tobcast client to broadcast messages that are delivered in
	 * total order. For clients that only wish to broadcast messages but
	 * not deliver them, one can specify {@code EXTERNAL_CLIENT} as the port
	 * number.
	 */
	private TobcastClient(LinkedList<IdIpPort> servers, int clientPort, AnerisType type, long startSlot,
		Selector selector) {
		this.servers = servers;
		indexNextMsg = new AtomicLong(startSlot);

		if (selector != null) {
			this.registeredSelector = selector;
		}
	}

	/**
	 * Creates a TobcastClient and specifies the first slot to deliver.
	 */
	public synchronized static TobcastClient newInstance(LinkedList<IdIpPort> servers, int clientPort,
		AnerisType type, long startSlot, Selector selector) throws IOException {

		TobcastClient client = new TobcastClient(servers, clientPort, type, startSlot, selector);

		synchronized(CLIENT_SLOTS) {
			CLIENT_SLOTS.add(client.indexNextMsg);
		}

		/**
		 * Start one thread that is responsible to receive msgs from Aneris
		 * and put them in the msg. buffer.
		 */
		if (!THREAD_STARTED) {
			DELIVERED_MSG_LOCK = new ReentrantLock();
			DELIVERED_MSG_COND = DELIVERED_MSG_LOCK.newCondition();

			ByteBuffer buffer = ByteBuffer.allocate(MAX_MSG_SIZE);
			SOCKETS = new SocketChannel[servers.size()];
			connectToServers(SOCKETS, servers, clientPort, buffer);
			ANERIS_TYPE = type;
			SELECTOR = Selector.open();

			THREAD_STARTED = true;
			
			client.start();
		}
		return client;
	}

	public static TobcastClient newInstance(LinkedList<IdIpPort> servers, int clientPort,
		AnerisType type) throws IOException {

		return newInstance(servers, clientPort, type, 1l, null);
	}

	public static TobcastClient newInstance(LinkedList<IdIpPort> servers, int clientPort,
		AnerisType type, Selector selector) throws IOException {

		return newInstance(servers, clientPort, type, 1l, selector);
	}

	public void closeSockets() {
		for (int i = 0; i < SOCKETS.length; i++) {
			try {
				if (SOCKETS[i] != null) {
					SOCKETS[i].close();
				}
				SOCKETS[i] = null;
			} catch (IOException ioe) {
				// There's not much to be done here...
			}
		}
	}

	/**
	 * This method connects to all the available servers.
	 */
	private static void connectToServers(SocketChannel[] sockets, LinkedList<IdIpPort> tobcastServers,
		int clientPort, ByteBuffer buf) {

		boolean atLeastOneSuccess = false;

		for (int i = 0; i < tobcastServers.size(); i++) {
			IdIpPort idIpPort = tobcastServers.get(i);

			try {
				SocketAddress socketAddress = new InetSocketAddress(idIpPort.getIp(), idIpPort.getPort());
				sockets[i] = SocketChannel.open();
				sockets[i].connect(socketAddress);
				atLeastOneSuccess = true;
				sockets[i].configureBlocking(false);
				sockets[i].socket().setTcpNoDelay(true);

				/**
				 * Send our port number only if we are an internal
				 * client. In that case, the client is declared in the
				 * configuration file.
				 */
				if (clientPort != EXTERNAL_CLIENT) {
					buf.clear();
					byte[] portBytes = (clientPort + "!").getBytes();
					buf.put(portBytes);
					buf.flip();
					NIOUtils.writeToChannel(sockets[i], buf);
				}

			} catch (Exception e) {
				// There's nothing to do here...
			}
		}

		if (!atLeastOneSuccess) {
			throw new IllegalStateException("Unable to connect to any TOBroadcast servers!");
		}
	}

	public void run() {
		HashSet<SelectionKey> keys = new HashSet<SelectionKey>();
		ByteBuffer byteBuffer = ByteBuffer.allocate(MAX_MSG_SIZE);
		long slotNextMsg = 1l;

		int[] batchSizeCount = new int[100];

		/**
		 * Registering the channels to do asynchronous I/O and
		 * make the primary connect to the backups.
		 */
		try {
			for (SocketChannel socket : SOCKETS) {
				SelectionKey key = socket.register(SELECTOR, SelectionKey.OP_READ);
				keys.add(key);
				key.attach(socket);
			}
		} catch (Exception e) {
			LOG.severe("Unable to init NIO for tobcast client, caught exception: " + e);
			return;
		}

		/**
		 * The main loop.
		 */
		for (;;) {
			boolean waitingForDelivery = false;

			try {
				int nbChannelsReady = SELECTOR.select();

				if (nbChannelsReady > 0) {
					Iterator<SelectionKey> it = SELECTOR.selectedKeys().iterator();

					while (it.hasNext()) {
						SelectionKey key = it.next();
						it.remove();
 
						/**
						 * Incoming messages to be delivered
						 */
						if (keys.contains(key)) {
							SocketChannel socketChannel = (SocketChannel) key.attachment();
							String rcvdMsg = (String) NIOUtils.readFromAnerisChannel(socketChannel, byteBuffer);

							/**
							 * Since it is enough to receive each command from only one replica, we first parse
							 * the slot number to avoid parsing the message multiple times.
							 */
							long slot = AnerisMessage.parseSlot(rcvdMsg, ANERIS_TYPE);

							if (slot >= slotNextMsg) {
								LinkedList<AnerisMessage> anerisMsg = AnerisMessage.parseString(rcvdMsg, slot, ANERIS_TYPE);
								batchSizeCount[anerisMsg.size()]++;

								if (DELIVERED_MSG_BUFF.putIfAbsent(slot, anerisMsg) == null) {
									if (slot == slotNextMsg) {
										waitingForDelivery = false;
										slotNextMsg++;

										if (registeredSelector != null) {
											registeredSelector.wakeup();
										}

										try {
											DELIVERED_MSG_LOCK.lock();
											DELIVERED_MSG_COND.signalAll();
										} finally {
											DELIVERED_MSG_LOCK.unlock();
										}
									}
								}
							}
						}
					}
				}

				// Garbage collect DELIVERED_MSG_BUFF
				if (slotNextMsg % 10000 == 0) {

					long minSlot = Long.MAX_VALUE;
					synchronized(CLIENT_SLOTS) {
						for (AtomicLong slot : CLIENT_SLOTS) {
							if (slot.get() < minSlot) {
								minSlot = slot.get();
							}
						}
					}

					for (long i = GC_SLOT; i < minSlot; i++) {
						DELIVERED_MSG_BUFF.remove(i);
					}
					GC_SLOT = minSlot;
				}

				if (!TOBCAST_MSG_BUF.isEmpty() && !waitingForDelivery) {
					AnerisMessage msgToBcast;

					while ((msgToBcast = TOBCAST_MSG_BUF.poll()) != null) {
						String nuprlProposal = AnerisMessage.toNuprlString(ANERIS_TYPE, msgToBcast.getType(), msgToBcast.getCmdId(),
							msgToBcast.nuprlProposal());
						sendMsgToAneris(nuprlProposal, byteBuffer);
						waitingForDelivery = true;
					}
				}
			} catch (Exception e) {
				LOG.warning("Caught exception: " + e + " in Tobcast client thread");
			}
		}
	}

	private void sendMsgToAneris(String msg, ByteBuffer byteBuffer) {
		boolean oneSuccessfullSend = false;

		// Add msg length
		msg = msg.length() + "!" + msg;

		byteBuffer.clear();
		byteBuffer.put(msg.getBytes());
		byteBuffer.flip();

		for (int i = 0; i < SOCKETS.length; i++) {
			if (SOCKETS[i] != null && SOCKETS[i].isConnected() && SOCKETS[i].isOpen()) {
				try {
					NIOUtils.writeToChannel(SOCKETS[i], byteBuffer);
					byteBuffer.rewind();
					oneSuccessfullSend = true;
				} catch (Exception e1) {

					// Close the stream.
					try {
						LOG.info("Caught exception: " + e1 + " closing tobcast client socket to " +
							SOCKETS[i].socket().getRemoteSocketAddress());

						SOCKETS[i].close();
						SOCKETS[i] = null;
					} catch (Exception e2) {
						// There's not much to be done here...
					}
				}
			}
		}
		if (!oneSuccessfullSend) {
			throw new IllegalStateException("Unable to TO-bcast message: " + msg);
		} 
	}

	/**
	 * This method broadcasts view change messages to databases. Delivery of
	 * these messages happen in total order.
	 */
	public void toBcast(AnerisMessage msg) {
		TOBCAST_MSG_BUF.add(msg);
		SELECTOR.wakeup();
	}

	/**
	 * This methods blocks until a message can be delivered.
	 * Delivery happens in total order and is gap free.
	 */
	public LinkedList<AnerisMessage> deliver() {

		long slot = indexNextMsg.get();
		LinkedList<AnerisMessage> msgsToDeliver = null;

		try {
			DELIVERED_MSG_LOCK.lock();
			do {
				msgsToDeliver = DELIVERED_MSG_BUFF.get(slot);
				if (msgsToDeliver == null) {
					DELIVERED_MSG_COND.await();
				}
			} while (msgsToDeliver == null);
		} catch (InterruptedException ie) {
			// There's not much to be done here...
		} finally {
			DELIVERED_MSG_LOCK.unlock();
		}

		LinkedList<AnerisMessage> otherMsgs = null;

		while ((otherMsgs = DELIVERED_MSG_BUFF.get(indexNextMsg.incrementAndGet())) != null) {
			msgsToDeliver.addAll(otherMsgs);
		}
		return msgsToDeliver;
	}

	/**
	 * This methods returns the sockets connected to the Aneris servers.
	 */
	public SocketChannel[] getSockets() {
		return SOCKETS;
	}
}
