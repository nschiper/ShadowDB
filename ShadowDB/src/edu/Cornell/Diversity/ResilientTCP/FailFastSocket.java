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
 *  o File name:   FailFastSocket.java
 *  o Description: A TCP Socket with failure detection capabilities.
 */

package edu.Cornell.Diversity.ResilientTCP;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.Pipe;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.Iterator;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Logger;

import edu.Cornell.Diversity.ShadowDB.ShadowDBConfig;
import edu.Cornell.Diversity.Utils.MessageParser;
import edu.Cornell.Diversity.Utils.NIOUtils;

/**
 * A fail-fast TCP socket implementation. This class uses
 * heart beats to detect the crash of the end point to which this socket
 * is connected. This class is not thread-safe: a single thread
 * must be invoking instance methods of this class.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class FailFastSocket extends Thread {

	private static final Logger LOG = Logger.getLogger("edu.Cornell.Diversity.Utils.FailFastSocket");

	public static final String UNKNOWN_ID = "Unknown ID";

	/**
	 * Default timeout when retrying to connect to a remote host
	 * or when trying to read the remote end point id.
	 */
	private static final int DEFAULT_TIMEOUT_MILLIS = 500;

	/**
	 * The default number of retries before considering
	 * the remote host dead. This is used only
	 * when connecting to the remote host and
	 * retrieving its id. After the connection
	 * is established, failure detection is done
	 * using heart beats and a timeout value
	 * defined in ShadowDBConfig.java.
	 */
	private static final int DEFAULT_RETRY_COUNT = 30;

	private final SocketChannel socketChannel;

	/**
	 * These are pipes that the client will respectively
	 * use to send messages and receive messages to and from
	 * the socket channel.
	 */
	private Pipe sendPipe;
	private Pipe rcvPipe;

	/**
	 * The selector used in the run() method.
	 */
	private Selector mainSelector;

	/**
	 * The selector used in the readObject() method.
	 */
	private Selector readObjSelector;

	private SelectionKey rcvPipeKey;

	/**
	 * Id of the end point we are connected to.
	 */
	private String endPointId;

	/**
	 * True iff we suspect the end point to have crashed.
	 */
	private AtomicBoolean endPointCrashed;

	/**
	 * A dummy object sent through the rcvPipe to notify
	 * of the cash suspicion of the process the socket is connected to.
	 * This is to make sure crash suspicions are detected
	 * even when the fail-fast socket is in non-blocking
	 * mode.
	 */
	private ByteBuffer crashSuspicion;

	/**
	 * A byte buffer to carry out transfers over NIO channels.
	 */
	private ByteBuffer byteBuffer;

	/**
	 * An object used to serialize messages.
	 */
	private ByteArrayOutputStream baos;

	/**
	 * An object used to parse messages received from the rcvPipe.
	 */
	private MessageParser rcvPipeParser;

	private void initObject(String localId, String remoteId) throws Exception {
		this.sendPipe = Pipe.open();
		this.sendPipe.source().configureBlocking(false);
		this.rcvPipe = Pipe.open();
		this.rcvPipe.source().configureBlocking(false);

		this.mainSelector = Selector.open();

		this.readObjSelector = Selector.open();
		this.rcvPipeKey = this.rcvPipe.source().register(readObjSelector, SelectionKey.OP_READ);

		this.endPointCrashed = new AtomicBoolean(false);
		this.crashSuspicion = ByteBuffer.allocate(NIOUtils.BYTE_COUNT_INT);
		this.crashSuspicion.putInt(MessageParser.SUSPICION);

		this.rcvPipeParser = new MessageParser(rcvPipe.source());

		/**
		 * Send our id.
		 */
		this.byteBuffer = ByteBuffer.allocate(ShadowDBConfig.getMaxMsgSize());
		this.baos = new ByteArrayOutputStream();
		NIOUtils.writeToChannel(this.socketChannel, localId, byteBuffer, baos);
		this.endPointId = null;

		/**
		 * Obtain the id of the other end point.
		 */
		for (int i = 0; i < DEFAULT_RETRY_COUNT && this.endPointId == null; i++) {
			this.endPointId = (String) NIOUtils.readObjFromChannel(socketChannel, byteBuffer, endPointCrashed);

			if (this.endPointId == null) {
				Thread.sleep(DEFAULT_TIMEOUT_MILLIS);
			}
		}

		if (this.endPointId == null) {
			throw new SuspectedCrashException(remoteId);
		}
	}

	private FailFastSocket(String ip, int port, String localId) throws Exception {
		SocketAddress socketAddress = new InetSocketAddress(ip, port);
		this.socketChannel = SocketChannel.open();
		this.socketChannel.configureBlocking(false);
		this.socketChannel.connect(socketAddress);
	}

	private FailFastSocket(SocketChannel channel) throws Exception {
		this.socketChannel = channel;
		this.socketChannel.configureBlocking(false);
	}

	public static FailFastSocket newInstance(SocketChannel channel, String localId, String remoteId)
		throws SuspectedCrashException {

		if (remoteId == null) {
			remoteId = UNKNOWN_ID;
		}

		try {
			FailFastSocket ffSocket = new FailFastSocket(channel);
			ffSocket.initObject(localId, remoteId);

			ffSocket.start();
			return ffSocket;
		} catch (Exception e) {
			throw new SuspectedCrashException(remoteId);
		}
	}

	public static FailFastSocket newInstance(String ip, int port, String localId, String remoteId)
		throws SuspectedCrashException {

		if (remoteId == null) {
			remoteId = UNKNOWN_ID;
		}

		FailFastSocket ffSocket = null;

		for (int i = 0; i < DEFAULT_RETRY_COUNT; i++) {
			try {
				if (i > 0) {
					Thread.sleep(DEFAULT_TIMEOUT_MILLIS);
				}

				if (ffSocket == null) {
					ffSocket = new FailFastSocket(ip, port, localId);
				}

				if (ffSocket.socketChannel.isOpen()) {
					if (ffSocket.socketChannel.isConnectionPending()) {
						ffSocket.socketChannel.finishConnect();
					}
					if (ffSocket.socketChannel.isConnected()) {
						ffSocket.initObject(localId, remoteId);
						ffSocket.start();
						return ffSocket;
					}
				} else {
					ffSocket.socketChannel.close();
					ffSocket = null;
				}
			} catch (Exception e) {
				LOG.warning("Unable to connect to: " + ip + ":" + port + " id: " + remoteId);
			}
		}

		throw new SuspectedCrashException(remoteId);
	}

	private long computeTimeout(long now, long lastRcvdMsgTS, long lastSentMsgTS, long heartBeatInterval) {
		long timeAfterRcvdMsg = now - lastRcvdMsgTS;
		long suspicionTimeout = Math.max(ShadowDBConfig.getHeartBeatTimeout() - timeAfterRcvdMsg, 0L);
		long timeAfterSentMsg = now - lastSentMsgTS;
		long sendHBTimeout = Math.max(heartBeatInterval - timeAfterSentMsg, 0L);
		long timeout = Math.min(sendHBTimeout, suspicionTimeout);

		/**
		 * A timeout of 0 means never timeout, we thus set it to a minimum of 1.
		 */
		timeout = Math.max(timeout, 1);
		return timeout;
	}

	public void run() {
		try {
			ByteBuffer heartBeat = ByteBuffer.allocate(NIOUtils.BYTE_COUNT_INT).putInt(MessageParser.HEARTBEAT_HEADER);
			MessageParser pipeParser = new MessageParser(sendPipe.source(), socketChannel);
			MessageParser socketParser = new MessageParser(socketChannel, rcvPipe.sink());

			Selector selector = Selector.open();
			SelectionKey socketKey = socketChannel.register(selector, SelectionKey.OP_READ);
			SelectionKey pipeKey = sendPipe.source().register(selector, SelectionKey.OP_READ);
			int msgHeader;

			long lastRcvdMsgTS = System.currentTimeMillis();
			long lastSentMsgTS = 0;

			int heartBeatInterval = ShadowDBConfig.getHeartBeatInterval();

			while (selector.isOpen() && !endPointCrashed.get()) {
				long now = System.currentTimeMillis();
				long timeout = computeTimeout(now, lastRcvdMsgTS, lastSentMsgTS, heartBeatInterval);

				int nbChannelsReady = selector.select(timeout);
				now = System.currentTimeMillis();

				// Suspect the end point to have crashed?
				if (lastRcvdMsgTS <= (now - ShadowDBConfig.getHeartBeatTimeout())) {
					System.out.println(System.currentTimeMillis() + " suspected " + endPointId + " because of timeout");
					notifyCrash();
					continue;
				}
				// Send a heart beat?
				else if (lastSentMsgTS <= (now - heartBeatInterval)) {
					heartBeat.rewind();
					NIOUtils.writeToChannel(socketChannel, heartBeat);
					lastSentMsgTS = now;
				}
				if (nbChannelsReady > 0) {
					Iterator<SelectionKey> it = selector.selectedKeys().iterator();

					while (it.hasNext()) {
						SelectionKey key = it.next();
						it.remove();

						if (key.equals(pipeKey) && key.isReadable()) {
							if (!pipeParser.expectingPayload()) {
								pipeParser.parseHeader();
							}

							if (pipeParser.expectingPayload()) {
								boolean transferComplete = pipeParser.transferMessage();
								if (transferComplete) {
									lastSentMsgTS = now;
								}
							}
						} else if (key.equals(socketKey) && key.isReadable()) {
							if (!socketParser.expectingPayload()) {
								msgHeader = socketParser.parseHeader();

								if (MessageParser.headerValid(msgHeader)) {
									lastRcvdMsgTS = now;
								}
							}
							if (socketParser.expectingPayload()) {
								socketParser.transferMessage();
							}
						}
						if (pipeParser.endPointSuspected() || socketParser.endPointSuspected()) {
							System.out.println(System.currentTimeMillis() + " suspected " + endPointId + " because read -1");

							notifyCrash();
						}
					}
				}
			}
		} catch (Exception e) {
			System.out.println(System.currentTimeMillis() + " suspected " + endPointId + " because of exception");

			/**
			 * If any exception is raised, we consider the other end to have crashed.
			 */
			notifyCrash();
		}
	}

	private void notifyCrash() {
		endPointCrashed.set(true);

		try {
			NIOUtils.writeToChannel(rcvPipe.sink(), crashSuspicion);
		} catch (IOException ioe) {
			// There's not much we can do here...
		}
	}

	/**
	 * Receives an object over the socket. It is assumed that, for a given FailFastSocket,
	 * only one thread calls any of the public methods.
	 * 
	 * @throws SuspectedCrashException if the process this socket is connected to
	 *         is suspected of having crashed.
	 */
	public Object readObject() throws Exception {
		Object msg = null;

        do {
        	int keysSelected = readObjSelector.select();

        	if (keysSelected > 0) {
        		Iterator<SelectionKey> it = readObjSelector.selectedKeys().iterator();

        		while (it.hasNext()) {
        			SelectionKey key = it.next();
        			it.remove();

        			if (key.equals(rcvPipeKey) && rcvPipeKey.isReadable()) {
        				if (!rcvPipeParser.expectingPayload()) {
        					rcvPipeParser.parseHeader();
        				}
        				if (rcvPipeParser.expectingPayload()) {
        					msg = rcvPipeParser.parseMessage();
        				}
        			}
        		}
        		if (rcvPipeParser.endPointSuspected()) {
            		throw new SuspectedCrashException(endPointId);
            	}
        	}
        } while (msg == null);

        return msg;
	}

	/**
	 * Sends object obj over the socket. It is assumed that, for a given FailFastSocket,
	 * only one thread calls any of the public methods.
	 * 
	 * @throws SuspectedCrashException if the process this socket is connected to
	 *         is suspected of having crashed.
	 */
	public void writeObject(Object obj) throws SuspectedCrashException {
		if (endPointCrashed.get()) {
			throw new SuspectedCrashException(endPointId);
		}

		try {
			if (!NIOUtils.writeToChannel(sendPipe.sink(), obj, byteBuffer, baos)) {
				LOG.severe("Sending too large objects, max size: " + ShadowDBConfig.getMaxMsgSize());
			}
		} catch (Exception e) {
			LOG.warning("Exception: " + e + "caught while sending object: " +
				" to: " + endPointId);
		}
	}

	public String getEndPointId() {
		return this.endPointId;
	}

	public void close() throws IOException {
		/**
		 * This stops the thread.
		 */
		endPointCrashed.set(true);
		mainSelector.wakeup();

		socketChannel.close();
		rcvPipe.sink().close();
		rcvPipe.source().close();
		sendPipe.sink().close();
		sendPipe.source().close();
	}

	public boolean isConnected() {
		return socketChannel.isConnected();
	}

	/**
	 * Registers the fail-fast socket to the selector for the given operations.
	 * This is useful when the fail-fast socket is used in non-blocking mode.
	 */
	public SelectionKey register(Selector selector, int operations) throws ClosedChannelException {
		return rcvPipe.source().register(selector, operations);
	}

	public String getRemoteAddress() {
		return socketChannel.socket().getRemoteSocketAddress().toString();
	}
}
