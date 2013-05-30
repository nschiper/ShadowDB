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
 *  o File name:   TobcastApp.java
 *  o Description: A class to test that the interface to Aneris works properly.
 */

package edu.Cornell.Diversity.Test;

import java.util.Collections;
import java.util.LinkedList;
import java.util.logging.Logger;

import edu.Cornell.Diversity.TOBroadcast.AnerisMessage;
import edu.Cornell.Diversity.TOBroadcast.AnerisMessage.PROTOCOL_TYPE;
import edu.Cornell.Diversity.TOBroadcast.TobcastClient;
import edu.Cornell.Diversity.TOBroadcast.TobcastClient.AnerisType;
import edu.Cornell.Diversity.Utils.ConfigurationParser;
import edu.Cornell.Diversity.Utils.IdIpPort;

/**
 * A class to test that the interface with the total order broadcast service Aneris
 * functions properly.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class TobcastApp extends Thread {

	private static final Logger LOG = Logger.getLogger("edu.Cornell.Diversity.Test.TobcastApp");

	private final int clientId;
	private final int bcastCount;
	private final TobcastClient tobcast;

	private float latency;
	private float throughput;
	private final LinkedList<Long> latencies;

	public TobcastApp(String configFile, int clientId, int bcastCount, PROTOCOL_TYPE protocol,
		AnerisType anerisType, boolean changeProtocol) {

		this.clientId = clientId;
		this.bcastCount = bcastCount;

		try {
			ConfigurationParser configParser = new ConfigurationParser(configFile);
			LinkedList<IdIpPort> tobcastServers = configParser.getToBCastServers();

			Integer clientPort = configParser.getPortFromId("client" + this.clientId);
			if (clientPort == null) {
				clientPort = TobcastClient.EXTERNAL_CLIENT;
			}
			tobcast = TobcastClient.newInstance(tobcastServers, clientPort, clientId, anerisType);
			configParser.closeConfigFile();

			latencies = new LinkedList<Long>();
			latency = 0f;
			throughput = 0f;

			if (changeProtocol) {
				AnerisMessage chgProtocol = new AnerisMessage(protocol, clientId);
				tobcast.toBcast(chgProtocol);
				LinkedList<AnerisMessage> confirmation = tobcast.deliver();
				LOG.info("Changed protocol to: " + confirmation);
			}
		} catch (Exception e) {
			throw new IllegalArgumentException("Unable to parse config. file: " + configFile +
				" caught exception: " + e);
		}
	}

	public void run() {
		AnerisMessage msg;
		String myId = "client" + clientId;
		String proposerId = "client" + clientId;
		boolean deliveredMyMsg = false;

		int slotNo = 1;
		int myDeliveredCount = 0;
		int totalMsgDelivered = 0;
		long start = System.currentTimeMillis();

		while (myDeliveredCount < bcastCount) {
			msg = new AnerisMessage(myId);

			long bcastTime = System.nanoTime();
			tobcast.toBcast(msg);

			do {
				// Deliver messages until it is ours.
				LinkedList<AnerisMessage> msgs = tobcast.deliver();

				if (msgs != null) {
					slotNo++;
					totalMsgDelivered += msgs.size();

					for (AnerisMessage delivered : msgs) {
						proposerId = delivered.getProposerId();

						if (proposerId.equals(myId)) {
							deliveredMyMsg = true;
							break;
						}
					}
				} else {
					LOG.warning("Delivered a null msg, exiting!");
					proposerId = "client" + clientId;
				}
			} while (!deliveredMyMsg);

			deliveredMyMsg = false;

			myDeliveredCount++;
			long deliverTime = System.nanoTime();
			latencies.add(deliverTime - bcastTime);
		}
		long end = System.currentTimeMillis();

		LOG.info("Client " + clientId + " broadcast " + myDeliveredCount + " msgs" +
				" average batch size: " + ((float) totalMsgDelivered) / ((float) slotNo));
		latency = ((float) (end - start)) / ((float) myDeliveredCount);
		throughput = ((float) myDeliveredCount) / ((float) (end-start)) * 1000f;
		LOG.info("Client " + clientId + " latency " + latency + " throughput " + throughput);
	}

	private void endExperiment() {
		tobcast.closeSockets();
	}

	/**
	 * Prints the 10, 50, 90, and 99 percentiles.
	 */
	private static void printLatPercentiles(LinkedList<Long> latencies) {

		Collections.sort(latencies);
	
		int index = 0;
		int tenthPercentileIndex = latencies.size() / 10;
		int medianIndex = latencies.size() / 2;
		int ninetiethPercentileIndex = (9 * latencies.size()) / 10;
		int ninenytNinePercentile = (99 * latencies.size()) / 100;
		int lastLatency = latencies.size() - 1;
		float avrgLatency = 0f;

		StringBuffer sb = new StringBuffer();
		for (long latency : latencies) {
			if (tenthPercentileIndex == index) {
				sb.append("\t 10% of latencies are below: " + (latency / 1000000) + " ms \n");
			} else if (medianIndex == index) {
				sb.append("\t 50% of latencies are below: " + (latency / 1000000) + " ms \n");
			} else if (ninetiethPercentileIndex == index) {
				sb.append("\t 90% of latencies are below: " + (latency / 1000000) + " ms \n");
			} else if (ninenytNinePercentile == index) {
				sb.append("\t 99% of latencies are below: " + (latency / 1000000) + " ms \n");
			} else if (lastLatency == index) {
				sb.append("\t max latency: " + (latency / 1000000) + " ms \n");
			}
			avrgLatency += ((float) latency / 1000000f);
			index++;
		}
		sb.append("Avrg latency is: " + avrgLatency / ((float) latencies.size()));
		LOG.info(sb.toString());
	}

	private static void printResults(TobcastApp[] clients, int clientCount) {
		float avrgLatency = 0f;
		float avrgThroughput = 0f;

		LinkedList<Long> allLatencies = new LinkedList<Long>();

		for (int i = 0; i < clients.length; i++) {
			avrgLatency += clients[i].latency;
			avrgThroughput += clients[i].throughput;
			allLatencies.addAll(clients[i].latencies);
		}

		avrgLatency /= (float) clientCount;

		StringBuffer sb = new StringBuffer();
		sb.append("**** Results with " + clientCount + " client(s) **** \n");
		sb.append("\t avrg. latency: " + avrgLatency + " ms \n");
		sb.append("\t avrg. throughput: " + avrgThroughput + " msgs/s \n");
		LOG.info(sb.toString());

		printLatPercentiles(allLatencies);
	}

	private static void runExp(TobcastApp[] clients, int clientCount) throws Exception {
		for (int i = 0; i < clientCount; i++) {
			clients[i].start();
		}

		for (int i = 0; i < clientCount; i++) {
			clients[i].join();
		}

		printResults(clients, clientCount);

		for (int i = 0; i < clients.length; i++) {
			clients[i].endExperiment();
		}
	}

	public static void main(String[] args) {
		try {
			if (args.length == 5) {
				String configFile = args[0];
				int clientCount = Integer.parseInt(args[1]);
				int bcastCount = Integer.parseInt(args[2]);
				AnerisType anerisType = AnerisType.valueOf(args[3]);
				PROTOCOL_TYPE protocol = PROTOCOL_TYPE.valueOf(args[4]);

				TobcastApp[] clients = new TobcastApp[clientCount];

				for (int i = 0; i < clientCount; i++) {
					if (i == 0) {
						clients[i] = new TobcastApp(configFile, i + 1, bcastCount, protocol, anerisType,
							true /* changeProtocol */);
					} else {
						clients[i] = new TobcastApp(configFile, i + 1, bcastCount, protocol, anerisType,
							false /* changeProtocol */);
					}
				}

				runExp(clients, clientCount);

			} else {
				System.err.println("Please specify the configuration file, the number of clients, the number of "
					+ " messages to broadcast, Aneris's type (INTERPRETED/LISP), "
					+ " and the protocol to use (PAXOS, TWOTHIRD)");
			}
		} catch (Exception e) {
			LOG.warning("Unable to start TobcastApp, caught exception: " + e);
		}
	}
}
