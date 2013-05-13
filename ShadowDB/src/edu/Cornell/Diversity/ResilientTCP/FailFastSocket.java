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

import java.io.IOException;
import java.io.Serializable;
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

	private static final long HEARTBEAT_HEADER = -1;

	private SocketChannel socketChannel;

	/**
	 * These are pipes that the client will respectively
	 * use to send messages and receive messages to and from
	 * the socket channel.
	 */
	private Pipe sendPipe;
	private Pipe rcvPipe;

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
	 * A preallocated byte buffer used to read and write from/to channels.
	 */
	private ByteBuffer byteBuffer;

	private void initObject(String id) throws Exception {
		this.sendPipe = Pipe.open();
		this.sendPipe.source().configureBlocking(false);
		this.rcvPipe = Pipe.open();
		this.rcvPipe.source().configureBlocking(false);

		this.endPointCrashed = new AtomicBoolean(false);

		EndPointCrashSuspicion crashSusipicion = new EndPointCrashSuspicion();
		this.crashSuspicion = ByteBuffer.allocate(NIOUtils.serializeObject(crashSusipicion).length);

		NIOUtils.serializeObject(crashSusipicion, crashSuspicion, false /* markEndOfHeader */);

		this.byteBuffer = ByteBuffer.allocate(ShadowDBConfig.getMaxMsgSize());			

		// Send our id
		NIOUtils.writeToChannel(this.socketChannel, id, byteBuffer);

		// Make sure we don't block forever waiting for the end point id.
		this.socketChannel.socket().setSoTimeout(ShadowDBConfig.getSocketTimeout());
		endPointId = (String) NIOUtils.readObjFromChannel(socketChannel, byteBuffer, endPointCrashed);

		// Set the channel to non-blocking mode
		this.socketChannel.configureBlocking(false);
	}

	/**
	 * Creates a fail-fast socket out of a SocketChannel. The id parameter
	 * denotes the id of this end point of the socket.
	 */
	private FailFastSocket(String ip, int port, String id) throws Exception {

		SocketAddress socketAddress = new InetSocketAddress(ip, port);
		this.socketChannel = SocketChannel.open();
		this.socketChannel.connect(socketAddress);

		initObject(id);
	}

	private FailFastSocket(SocketChannel channel, String id) throws Exception {
		this.socketChannel = channel;

		initObject(id);
	}

	public static FailFastSocket newInstance(SocketChannel channel, String remoteId)
		throws SuspectedCrashException {

		try {
			FailFastSocket ffSocket = new FailFastSocket(channel, remoteId);
	
			ffSocket.start();
			return ffSocket;
		} catch (Exception sce) {
			throw new SuspectedCrashException(remoteId);
		}
	}

	public static FailFastSocket newInstance(String ip, int port, String remoteId, String localId)
		throws SuspectedCrashException {

		try {
			FailFastSocket ffSocket = new FailFastSocket(ip, port, localId);
	
			ffSocket.start();
			return ffSocket;
		} catch (Exception sce) {
			throw new SuspectedCrashException(remoteId);
		}
	}

	public static FailFastSocket newInstanceWithRetries(String ip, int port, String remoteId, String localId,
		int noRetrials) throws SuspectedCrashException {

		FailFastSocket ffSocket = null;
		boolean connected = false;

		for (int i = 0; i < noRetrials && !connected; i++) {
			try {
				ffSocket = new FailFastSocket(ip, port, localId);
				connected = true;
			} catch (Exception e) {
				try {
					// Sleep before retrying
					if (i < (noRetrials - 1)) {
						Thread.sleep(ShadowDBConfig.getSocketTimeout());
					}
				} catch (InterruptedException ie) {
					// Do nothing...
				}
			}
		}
		if (!connected) {
			throw new SuspectedCrashException(remoteId);
		} else {
			ffSocket.start();
			return ffSocket;
		}
	}

	private long computeTimeout(long now, long lastRcvdMsgTS, long lastSentMsgTS, long heartBeatInterval) {
		long timeAfterRcvdMsg = now - lastRcvdMsgTS;
		long suspicionTimeout = Math.max(ShadowDBConfig.getSocketTimeout() - timeAfterRcvdMsg, 0L);
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
			ByteBuffer heartBeat = ByteBuffer.allocate(NIOUtils.NO_BYTES_LONG).putLong(HEARTBEAT_HEADER);
			ByteBuffer msgBuffer = ByteBuffer.allocate(ShadowDBConfig.getMaxMsgSize());

			Selector selector = Selector.open();
			SelectionKey socketKey = socketChannel.register(selector, SelectionKey.OP_READ);
			SelectionKey pipeKey = sendPipe.source().register(selector, SelectionKey.OP_READ);

			long lastRcvdMsgTS = System.currentTimeMillis();
			long lastSentMsgTS = 0;

			int heartBeatInterval = ShadowDBConfig.getHeartBeatInterval();

			while (selector.isOpen() && !endPointCrashed.get()) {
				long now = System.currentTimeMillis();
				long timeout = computeTimeout(now, lastRcvdMsgTS, lastSentMsgTS, heartBeatInterval);

				int nbChannelsReady = selector.select(timeout);
				now = System.currentTimeMillis();

				// Suspect the end point to have crashed?
				if (lastRcvdMsgTS <= (now - ShadowDBConfig.getSocketTimeout())) {
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
						boolean transferOK = true;
						long header = 0;

						SelectionKey key = it.next();
						it.remove();
 
						if (!key.isValid()) {
							continue;
						}

						if (key.equals(pipeKey) && key.isReadable()) {
							lastSentMsgTS = now;
							header = NIOUtils.readHeader(sendPipe.source(), msgBuffer);

							if (header != NIOUtils.ERROR_HEADER_LONG) {
								transferOK = NIOUtils.transferData(header, sendPipe.source(), socketChannel, msgBuffer);
							}
						} else if (key.equals(socketKey) && key.isReadable()) {
							lastRcvdMsgTS = now;
							header = NIOUtils.readHeader(socketChannel, msgBuffer);

							if (header != HEARTBEAT_HEADER && header != NIOUtils.ERROR_HEADER_LONG) {
								transferOK = NIOUtils.transferData(header, socketChannel, rcvPipe.sink(), msgBuffer);
								synchronized(rcvPipe.sink()) {
									rcvPipe.sink().notify();
								}
							}
						}
						if (header == NIOUtils.ERROR_HEADER_LONG || !transferOK) {
							notifyCrash();
						}
					}
				}
			}
		} catch (Exception e) {
			// If any exception is raised, we consider the other end to have crashed.
			notifyCrash();
			LOG.fine("Exception: \"" + e + "\" caught in fail fast socket connected to: " + endPointId);
		}
	}

	private void notifyCrash() {
		endPointCrashed.set(true);

		try {
			crashSuspicion.rewind();
			NIOUtils.writeToChannel(rcvPipe.sink(), crashSuspicion);
		} catch (IOException ioe) {
			// There's not much we can do here...
		}

		synchronized(rcvPipe.sink()) {
			rcvPipe.sink().notify();
		}
	}

	/**
	 * Receives an object over the socket. It is assumed that, for a given FailFastSocket,
	 * only one thread calls any of the public methods.
	 * 
	 * @throws SuspectedCrashException if the process this socket is connected to
	 *         is suspected of having crashed.
	 */
	public Object readObject() throws SuspectedCrashException {
		Object msg = null;

		do {
			try {
				synchronized (rcvPipe.sink()) {
					msg = NIOUtils.readObjFromChannel(rcvPipe.source(), byteBuffer, endPointCrashed);
					if (msg == null) {
						rcvPipe.sink().wait();
					}
				}
			} catch (Exception e) {
				LOG.warning("Exception: " + e + "caught while receiving object: " +
					" sent from: " + endPointId);
			}
		} while (msg == null && !endPointCrashed.get());

		if (endPointCrashed.get()) {
			throw new SuspectedCrashException(endPointId);
		}
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
			if (!NIOUtils.writeToChannel(sendPipe.sink(), obj, byteBuffer)) {
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

	/**
	 * A dummy class that is sent through the rcvPipe to notify the suspicion
	 * of the crash of process the socket is connected to.
	 */
	private static class EndPointCrashSuspicion implements Serializable {
		private static final long serialVersionUID = 1L;
	}
}
