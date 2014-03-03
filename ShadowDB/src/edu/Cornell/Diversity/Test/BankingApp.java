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
 *  o File name:   BankingApp.java
 *  o Description: A simple banking application to test ShadowDB.
 */

package edu.Cornell.Diversity.Test;

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import edu.Cornell.Diversity.ShadowDB.DBConnection;
import edu.Cornell.Diversity.ShadowDB.ShadowDBSMRConnection;
import edu.Cornell.Diversity.ShadowDB.ShadowDbPBConnection;
import edu.Cornell.Diversity.ShadowDB.ShadowTransaction;
import edu.Cornell.Diversity.ShadowDB.TransactionId;
import edu.Cornell.Diversity.TOBroadcast.AnerisMessage;
import edu.Cornell.Diversity.TOBroadcast.TobcastClient;
import edu.Cornell.Diversity.TOBroadcast.TobcastClient.AnerisType;
import edu.Cornell.Diversity.Utils.ConfigurationParser;
import edu.Cornell.Diversity.Utils.IdIpPort;
import edu.Cornell.Diversity.Utils.TransactionIdGenerator;

/**
 * A simple banking application to test that ShadowDB works properly.
 * The database consists of a set of accounts, and the clients deposit,
 * withdraw, and transfer money.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class BankingApp extends Thread {

	private static final Logger LOG = Logger.getLogger("edu.Cornell.Diversity.Test.BankingApp");

	public static final String TABLE_NAME = "ACCOUNTS";

	public static final int ACCOUNT_COUNT = 50000;

	private static enum CLIENT_TYPE {POPULATE, WARMUP, BENCHMARK};

	private final String clientId;

	/**
	 * Average throughput and latency of the executed
	 * transactions.
	 */
	private float averageThroughput;
	private float averageLatency;

	private final LinkedList<Long> latencies;

	/**
	 * True if read-only transactions will be executed
	 */
	private final boolean readOnly;

	private final AtomicInteger noExecutedTrans;

	private AnerisType anerisType;

	/**
	 * A connection to ShadowDB when replicated in a primary-backup fashion.
	 */
	private DBConnection dbConnection;

	public BankingApp(int clientNo, String configFile, boolean readOnly,
		AnerisType anerisType) throws Exception {

		this.clientId = "BankingApp-" + clientNo;
		this.readOnly = readOnly;
		this.noExecutedTrans = new AtomicInteger(0);
		this.latencies = new LinkedList<Long>();
		this.anerisType = anerisType;

		ConfigurationParser configParser = new ConfigurationParser(configFile);
		LinkedList<IdIpPort> allDbs = configParser.getDbServers();

		if (anerisType == AnerisType.INTERPRETED) {
			this.dbConnection = new ShadowDbPBConnection(clientId, allDbs);
		} else {
			LinkedList<IdIpPort> tobcastServers = configParser.getToBCastServers();

			Integer clientPort = configParser.getPortFromId("client" + this.clientId);
			if (clientPort == null) {
				clientPort = TobcastClient.EXTERNAL_CLIENT;
			}
			TobcastClient tobcast = TobcastClient.newInstance(tobcastServers, clientPort, anerisType);
			this.dbConnection = new ShadowDBSMRConnection(clientId, allDbs, tobcast);
		}
		configParser.closeConfigFile();
	}

	public float getThroughtput() {
		return this.averageThroughput;
	}

	private float getLatency() {
		return this.averageLatency;
	}

	private ShadowTransaction generateShadowTrans(boolean readOnly,
		TransactionIdGenerator idGenerator, Random rnd) {

		TransactionId id = idGenerator.getNextId();
		ShadowTransaction t;

		if (readOnly) {
			int[] rndAccounts = {rnd.nextInt(ACCOUNT_COUNT)};

			t = new BalanceTransaction(id, rndAccounts);
		} else {
			int[] rndAccounts = {rnd.nextInt(ACCOUNT_COUNT)};
			int amount = rnd.nextInt(100000);
			t = new DepositTransaction(id, rndAccounts, amount);
		}
		return t;
	}

	private AnerisMessage generateMiniTrans(boolean readOnly, Random rnd) {

		LinkedList<Integer> keysToRead = new LinkedList<Integer>();
		HashMap<Integer, Integer> keysToWrite = new HashMap<Integer, Integer>();

		if (readOnly) {
			keysToRead.add(rnd.nextInt(ACCOUNT_COUNT));
		} else {
			keysToWrite.put(rnd.nextInt(ACCOUNT_COUNT), rnd.nextInt(10000));
		}
		return new AnerisMessage(clientId, keysToRead, keysToWrite);
	}

	public void run() {

		int nbTransToExec;

		if (anerisType == AnerisType.INTERPRETED) {
			nbTransToExec = 10000;
		} else {
			nbTransToExec = 100;
		}

		TransactionIdGenerator idGenerator = new TransactionIdGenerator();

		Random rnd = new Random(System.currentTimeMillis());
		long start = System.currentTimeMillis();

		for (int i = noExecutedTrans.get(); i < nbTransToExec; i++) {
			long startExec = System.currentTimeMillis();

			if (anerisType == AnerisType.INTERPRETED) {
				ShadowTransaction toExecute = generateShadowTrans(readOnly, idGenerator, rnd);
				dbConnection.submit(toExecute);
			} else {
				AnerisMessage toExecute = generateMiniTrans(readOnly, rnd);
				dbConnection.submit(toExecute);
			}
			long endExec = System.currentTimeMillis();
			latencies.add(endExec - startExec);

			noExecutedTrans.set(i);

			if (i % 100 == 0) {
				LOG.info(clientId + ": submitted " + i + " transactions");
			}
		}

		LOG.info("Client: " + clientId + " done executing transactions!");

		long end = System.currentTimeMillis();
		long totalTime = end - start;
		this.averageThroughput = ((float) nbTransToExec) / ((float) totalTime) * 1000f;
		this.averageLatency = ((float) totalTime) / ((float) nbTransToExec);

		try {
			dbConnection.close();

		} catch (Exception e) {
			LOG.warning("Unable to close connection to database, caught exception: " + e);
		}
	}

	/**
	 * Prints the 10, 50, 90, and 99 percentiles.
	 */
	private static void printLatencyPercentiles(LinkedList<Long> latencies) {

		Collections.sort(latencies);
	
		int index = 0;
		int tenthPercentileIndex = latencies.size() / 10;
		int medianIndex = latencies.size() / 2;
		int ninetiethPercentileIndex = (9 * latencies.size()) / 10;
		int ninenytNinePercentile = (99 * latencies.size()) / 100;

		StringBuffer sb = new StringBuffer();

		for (long latency : latencies) {
			if (tenthPercentileIndex == index) {
				sb.append("\t 10% of latencies are below: " + latency + "\n");
			} else if (medianIndex == index) {
				sb.append("\t 50% of latencies are below: " + latency + "\n");				
			} else if (ninetiethPercentileIndex == index) {
				sb.append("\t 90% of latencies are below: " + latency + "\n");	
			} else if (ninenytNinePercentile == index) {
				sb.append("\t 99% of latencies are below: " + latency + "\n");
			}
			index++;
		}
		LOG.info(sb.toString());
	}

	private static void runClients(String configFile, int clientCount, CLIENT_TYPE clientType,
		boolean readOnly, AnerisType anerisType,
		boolean showResults) throws Exception {

		BankingApp[] clients = new BankingApp[clientCount];

		for (int i = 0; i < clientCount; i++) {
			clients[i] = new BankingApp(i + 1, configFile, readOnly, anerisType);
			clients[i].start();
		}

		if (clientType == CLIENT_TYPE.BENCHMARK) {
			LOG.info("Started experiment with " + clientCount + " clients");
		} else if (clientType == CLIENT_TYPE.WARMUP) {
			LOG.info("Warming up database with " + clientCount + " clients");
		}

		for (int i = 0; i < clientCount; i++) {
			clients[i].join();
		}

		if (showResults) {
			float latency = 0f;
			float throughput = 0f;

			LinkedList<Long> allLatencies = new LinkedList<Long>();

			for (int i = 0; i < clientCount; i++) {
				latency += clients[i].getLatency() / ((float) clientCount);
				throughput += clients[i].getThroughtput();
				allLatencies.addAll(clients[i].latencies);
			}
			String operationType = (readOnly ? "read-only" : "update");
			StringBuffer sb = new StringBuffer();
			sb.append(" *** Benchmark results for " + clientCount + " " + operationType + " clients:\n");
			sb.append("\t throughput: " + throughput + " TPS, avrg. latency: " + latency + " ms");
			LOG.info(sb.toString());

			Collections.sort(allLatencies);
			printLatencyPercentiles(allLatencies);
		}
	}

	public void populateDb() {
		TransactionId id = new TransactionId("myIp", Thread.currentThread().getId(), 0);

		try {
			if (anerisType == AnerisType.INTERPRETED) {
				PopulateBankDBTransaction populateTrans = new PopulateBankDBTransaction(id);
				dbConnection.submit(populateTrans);
			} else {
				LinkedList<Integer> keysToRead = new LinkedList<Integer>();
				HashMap<Integer, Integer> keysToWrite = new HashMap<Integer, Integer>();
				
				AnerisMessage populateTrans = new AnerisMessage(clientId, keysToRead, keysToWrite);
				dbConnection.submit(populateTrans);
			}
		} catch (Exception e) {
			LOG.warning("Unable to populate bank database, caught exception: " + e);
		}
		LOG.info("Successfully populated the database");
	}

	public static void main(String[] args) {
		try {
			if (args.length == 6) {
				String configFile = args[0];
				boolean readOnly = args[1].equals("read-only");

				int minClientCount = Integer.parseInt(args[2]);
				int maxClientCount = Integer.parseInt(args[3]);
				int clientIncrement = Integer.parseInt(args[4]);

				AnerisType anerisType = AnerisType.valueOf(args[5]);

				String config = "Started Banking app with parameters: \n"
					+ "\t config file: " + configFile + "\n"
					+ "\t read only: " + readOnly + "\n"
					+ "\t account count: " + ACCOUNT_COUNT + "\n"
					+ "\t min clients: " + minClientCount + "\n"
					+ "\t max client: " + maxClientCount + "\n"
					+ "\t clientIncrement: " + clientIncrement + "\n"
					+ "\t aneris's type: " + anerisType;
				LOG.info(config);

				/**
				 * Populate database.
				 */
				BankingApp app = new BankingApp(1 /*clientNo */, configFile, readOnly, anerisType);
				app.populateDb();
				app.dbConnection.close();

				/**
				 * Warm up database.
				 */
				runClients(configFile, 5 /* clientCount */, CLIENT_TYPE.WARMUP,
					false /* read-only? */, anerisType, false /* showResults */);

				/**
				 * Run benchmark.
				 */
				for (int i = minClientCount; i <= maxClientCount; i += clientIncrement) {
					runClients(configFile, i /* clientCount */, CLIENT_TYPE.BENCHMARK,
						readOnly, anerisType, true /* showResults */);
				}
			} else {
				System.err.println("Please specify a configuration file, "
					+ ", whether to issue read-only transactions or not (read-only/update)"
					+ ", the number of rows the ACCOUNT table should contain"
					+ ", the min/max client count, the client count increment,"
					+ " and aneris's type (INTERPRETED/LISP).");
			}
		} catch (Exception e) {
			LOG.warning("Unable to start BankingApp, caught exception: " + e);
		}		
	}
}
