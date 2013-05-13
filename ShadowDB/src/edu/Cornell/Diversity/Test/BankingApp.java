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
import java.util.LinkedList;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import edu.Cornell.Diversity.ShadowDB.DBConnection;
import edu.Cornell.Diversity.ShadowDB.ShadowDBConnection;
import edu.Cornell.Diversity.ShadowDB.ShadowTransaction;
import edu.Cornell.Diversity.ShadowDB.TransactionId;
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

	private static final Logger LOG = Logger.getLogger("edu.Cornell.Diversity.Client.BankingApp");

	public static final String TABLE_NAME = "ACCOUNTS";

	private static enum CLIENT_TYPE {POPULATE, WARMUP, BENCHMARK};

	private final String clientId;

	/**
	 * Average throughput and latency of the executed
	 * transactions.
	 */
	private float averageThroughput;
	private float averageLatency;

	private final LinkedList<Long> latencies;

	private final CLIENT_TYPE clientType;

	/**
	 * True if read-only transactions will be executed
	 */
	private final boolean readOnly;

	private final int accountCount;

	private final AtomicInteger noExecutedTrans;

	/**
	 * A connection to ShadowDB when replicated in a primary-backup fashion.
	 */
	private DBConnection dbConnection;

	public BankingApp(int clientNo, String configFile, CLIENT_TYPE clientType,
		boolean readOnly, int accountCount) throws Exception {

		this.clientId = "BankingApp-" + clientNo;
		this.clientType = clientType;
		this.readOnly = readOnly;
		this.accountCount = accountCount;
		this.noExecutedTrans = new AtomicInteger(0);
		this.latencies = new LinkedList<Long>();
		ConfigurationParser configParser = new ConfigurationParser(configFile);
		LinkedList<IdIpPort> allDbs = configParser.getDbServers();
		this.dbConnection = new ShadowDBConnection(clientId, allDbs);
		configParser.closeConfigFile();
	}

	public float getThroughtput() {
		return this.averageThroughput;
	}

	private float getLatency() {
		return this.averageLatency;
	}

	private ShadowTransaction generateNextTransaction(boolean readOnly,
		TransactionIdGenerator idGenerator, Random rnd) {

		TransactionId id = idGenerator.getNextId();
		ShadowTransaction t;

		if (readOnly) {
			int[] rndAccounts = {rnd.nextInt(accountCount)};

			t = new BalanceTransaction(id, rndAccounts);
		} else {
			int[] rndAccounts = {rnd.nextInt(accountCount)};
			int amount = rnd.nextInt(100000);
			t = new DepositTransaction(id, rndAccounts, amount);
		}
		return t;
	}

	public void run() {

		int nbTransToExec;

		if (readOnly) {
			nbTransToExec = 30000;
		} else if (clientType == CLIENT_TYPE.WARMUP) {
			nbTransToExec = 10000;
		} else {
			nbTransToExec = 10000;				
		}

		TransactionIdGenerator idGenerator = new TransactionIdGenerator();

		if (clientType == CLIENT_TYPE.POPULATE) {
			PopulateBankDBTransaction populateTrans = new PopulateBankDBTransaction(idGenerator.getNextId(), accountCount);

			try {
				dbConnection.submit(populateTrans);
			} catch (Exception e) {
				LOG.warning("Unable to populate bank database, caught exception: " + e);
			}
			LOG.info("Successfully populated the database");

		} else {
			Random rnd = new Random(System.currentTimeMillis());
			long start = System.currentTimeMillis();
			ShadowTransaction toExecute = null;
	
			while (noExecutedTrans.get() < nbTransToExec) {
				try {
					for (int i = noExecutedTrans.get(); i <= nbTransToExec; i++) {
						if (toExecute == null) {
							toExecute = generateNextTransaction(readOnly, idGenerator, rnd);
						}
						long startExec = System.currentTimeMillis();
						dbConnection.submit(toExecute);
						long endExec = System.currentTimeMillis();
						latencies.add(endExec - startExec);

						toExecute = null;

						noExecutedTrans.set(i);
					}
				} catch (Exception e) {
					LOG.warning("Caught exception: " + e + " in BankingApp");
				}
			}
			long end = System.currentTimeMillis();
			long totalTime = end - start;
			this.averageThroughput = ((float) nbTransToExec) / ((float) totalTime) * 1000f;
			this.averageLatency = ((float) totalTime) / ((float) nbTransToExec);
		}

		try {
			dbConnection.close();

		} catch (Exception e) {
			LOG.warning("Unable to close connection to database, caught exception: " + e);
		}
	}

	/**
	 * Prints the 10, 50, and 90 percentiles.
	 */
	private static void printLatPercentiles(LinkedList<Long> latencies) {

		Collections.sort(latencies);
	
		int index = 0;
		int tenthPercentileIndex = latencies.size() / 10;
		int medianIndex = latencies.size() / 2;
		int ninetiethPercentileIndex = (9 * latencies.size()) / 10;
		int ninenytNinePercentile = (99 * latencies.size()) / 100;

		for (long latency : latencies) {
			if (tenthPercentileIndex == index) {
				LOG.info("\t 10% of latencies are below: " + latency);
			} else if (medianIndex == index) {
				LOG.info("\t 50% of latencies are below: " + latency);				
			} else if (ninetiethPercentileIndex == index) {
				LOG.info("\t 90% of latencies are below: " + latency);				
			} else if (ninenytNinePercentile == index) {
				LOG.info("\t 99% of latencies are below: " + latency);
			}
			index++;
		}
	}

	private static void runClients(String configFile, int clientCount, CLIENT_TYPE clientType,
		boolean readOnly, int accountCount, boolean showResults) throws Exception {

		BankingApp[] clients = new BankingApp[clientCount];

		for (int i = 0; i < clientCount; i++) {
			clients[i] = new BankingApp(i, configFile, clientType, readOnly, accountCount);
			clients[i].start();
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
			LOG.info(" *** Benchmark results for " + clientCount + " " + operationType + " clients:");
			LOG.info("\t throughput: " + throughput + " TPS, avrg. latency: " + latency + " ms");

			Collections.sort(allLatencies);
			printLatPercentiles(allLatencies);
		}
	}

	public static void main(String[] args) {
		try {
			if (args.length == 6) {
				String configFile = args[0];
				boolean readOnly = args[1].equals("read-only");
				int accountCount = Integer.parseInt(args[2]);

				int minClientCount = Integer.parseInt(args[3]);
				int maxClientCount = Integer.parseInt(args[4]);
				int clientIncrement = Integer.parseInt(args[5]);

				LOG.info("Started Banking app with parameters: ");
				LOG.info("\t config file: " + configFile);
				LOG.info("\t read only: " + readOnly);
				LOG.info("\t acount count: " + accountCount);
				LOG.info("\t min clients: " + minClientCount);
				LOG.info("\t max client: " + maxClientCount);
				LOG.info("\t clientIncrement: " + clientIncrement);

				/**
				 * Warm up database.
				 */
				runClients(configFile, 5 /* clientCount */, CLIENT_TYPE.WARMUP,
					false /* read-only? */, accountCount, false /* showResults */);

				for (int i = minClientCount; i <= maxClientCount; i += clientIncrement) {
					runClients(configFile, i /* clientCount */, CLIENT_TYPE.BENCHMARK,
						readOnly, accountCount, true /* showResults */);
				}
			} else {
				System.err.println("Please specify a configuration file, "
					+ ", whether to issue read-only transactions or not (read-only/update)"
					+ ", the number of rows the ACCOUNT table should contain"
					+ ", the min/max client count, and the client count increment.");
			}
		} catch (Exception e) {
			LOG.warning("Unable to start BankingApp, caught excpetion: " + e);
		}		
	}
}