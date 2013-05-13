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
 *  o File name:   ShadowDB.java
 *  o Description: The ShadowDB replication protocol.
 */

package edu.Cornell.Diversity.ShadowDB;

import java.nio.channels.Selector;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.logging.Logger;

import edu.Cornell.Diversity.ResilientTCP.FailFastSocket;
import edu.Cornell.Diversity.ShadowDB.ShadowDBConfig.TRANS_MODE_TYPE;
import edu.Cornell.Diversity.Utils.ConfigurationParser;
import edu.Cornell.Diversity.Utils.DbUtils;
import edu.Cornell.Diversity.Utils.IdIpPort;
import edu.Cornell.Diversity.Utils.ShutdownHook;
import edu.Cornell.Diversity.Utils.TransactionIdGenerator;
import edu.Cornell.Diversity.Utils.DbUtils.DB_TYPE;

/**
 * This class implements the ShadowDB replication protocol,
 * based on the primary-backup approach and works as follows:
 * 
 * -Database replicas form a group. The group has a primary that executes
 *  transactions and forwards updates to the backups. Once the primary
 *  received acknowledgment from all backups, thereby certifying that
 *  they persisted the updates, the primary commits the transaction locally
 *  and replies to the client with the transaction's result.
 *
 * -In case of a suspected failures of one of the replicas, the primary times out
 *  and aborts the transaction. The suspected failed database is replaced by a fresh
 *  one by the group (see Group.java). This class maintains the group membership and ensures that
 *  databases observe the same sequence of group memberships by relying on a total
 *  order broadcast service called Aneris.
 *  
 *  -To ensure correct execution when failures occur, we require that
 *  once a group member suspects a failure it "wedges" itself and stops executing
 *  transactions (if it is the primary) or stops applying updates (if it is a backup).
 *  This ensures that the current group cannot execute transactions anymore. To ensure
 *  liveness, a new group membership will be installed where the replica suspected of having
 *  crashed is replaced by a fresh replica. To ensure backups start in the same state 
 *  as the primary, the primary transfers its state to the backups.
 *  
 * -Transactions submitted to the primary are a subclass of ShadowTransaction.java. Transactions
 *  either implement the executeSQL method of the ShadowTransaction class or the executeQuery
 *  and executeUpdate methods. In the first case, update transactions are executed both
 *  on the primary and on the backups. In the second case, only the primary executes "executeQuery".
 *  The primary and backups execute "executeUpdate". It must be the case that after "executeQuery"
 *  has been invoked, "executeUpdate" is fully parameterized. This is typically accomplished
 *  by storing parameters of the update phase as instance variables. 
 * 
 *  The replication protocol assumes a crash model where replicas may crash at any point in time. 
 *  To tolerate up to f replica failures, f+1 databases are deployed. The protocol assumes that there will always be
 *  at least one replica that never crashes. Hence, total failure of the replicas followed by a
 *  recovery is not currently supported.
 *  
 * @author nschiper@cs.cornell.edu
 *
 */
public class ShadowDBServer {

	private static final Logger LOG = Logger.getLogger("edu.Cornell.Diversity.ShadowDB.ShadowDBServer");

	private static final ConcurrentLinkedQueue<ShadowTransaction> TRANS_TO_EXECUTE =
		new ConcurrentLinkedQueue<ShadowTransaction>();

	/**
	 * Transactions that have been executed by all group members, per sequence number.
	 */
	private static final ConcurrentHashMap<TransactionId, QueryResult> TRANS_EXECUTED =
		new ConcurrentHashMap<TransactionId, QueryResult>(200000 /* initial capacity */);

	/************** The following variables are used by the primary. ****************/

	/**
	 * The acknowledgments received by the primary.
	 */
	private final HashMap<TransactionId, HashSet<String>> acknowledgments;

	/**
	 * A map of transactiont ids that come back as acknowledgments (from backups)
	 * to transaction ids on which the client handler waits to be notified
	 * when the answer is ready to be sent to the corresponding client.
	 */
	private final HashMap<TransactionId, TransactionId> id2IdMap;

	/**
	 * A hashmap used to garbage collect the TRANS_EXECUTED hashtable. Transaction ids
	 * are inserted along with the time of insertion. Transactions older than GC_TIMEOUT_MILLIS
	 * milliseconds will be garbage collected from TRANS_EXECUTED and toGarbageCollect.
	 */
	private final LinkedHashMap<Long, TransactionId> toGC;

	private final TransactionIdGenerator idGenerator;

	
	/********* The following variables are used by the primary and the backups. ************/

	/**
	 * Timeout in seconds after which transaction results are garbage collected.
	 * One must ensure that the likelihood of garbage collecting the transaction
	 * results before the client receives them is very low.
	 * Otherwise, a transaction may be executed twice when the primary database
	 * fails before sending the results to the client, and the client resubmits
	 * the transaction although backups have executed the transaction already. 
	 */
	private final int gcTimeoutMillis;

	/**
	 * When the transaction mode is set to NORMAL, transactions extending class
	 * ShadowTransaction must implement executeSQL. When set to TWO_PHASE,
	 * executeQuery and executeUpdat must be implemented. The query phase is
	 * only executed on the primary, the update phase is executed both
	 * on the primary and the backups. The query phase must fully
	 * determine the update phase. This is typically accomplished
	 * using instance variables.
	 */
	private final TRANS_MODE_TYPE transMode;

	/**
	 * Connection to the SQL database.
	 */
	private Connection connection;

	/**
	 * Each transaction executed by the primary gets a sequence number. This
	 * is used to totally order transactions. The variable below is used
	 * to assign a sequence number to the next executed transaction.
	 */
	private long nextSeqNo;

	/**
	 *  The group of replicas.
	 */
	private Group group;

	private final boolean replicated;
	private final DB_TYPE dbType;
	private final String dbId;

	private ShadowDBServer(String dbId, int dbPort, boolean replicated, DB_TYPE dbType,
		int nextSeqNo, LinkedList<IdIpPort> allDbs, LinkedList<IdIpPort> tobcastServers,
		int clientPort) throws Exception {

	    this.dbType = dbType;
	    this.dbId = dbId;
	    this.replicated = replicated;
	    this.idGenerator = new TransactionIdGenerator();
		this.connection = DbUtils.openDbSql(dbType);

	    this.nextSeqNo = nextSeqNo;
	    this.acknowledgments = new HashMap<TransactionId, HashSet<String>>(100000);
	    this.id2IdMap = new HashMap<TransactionId, TransactionId>(100000);

	    this.transMode = ShadowDBConfig.getTransactionMode();

	    this.gcTimeoutMillis = ShadowDBConfig.getGcTimeoutMillis();
	    this.toGC = new LinkedHashMap<Long, TransactionId>();
		this.group = new Group(allDbs, tobcastServers, this, dbPort, clientPort);

		LOG.info(dbIdToString() + " started replica");
	}

	public void closeDb() throws SQLException {
        if (connection != null) {
        	connection.close();
        }
	}

	public boolean isReplicated() {
		return replicated;
	}

	public String getDbId() {
		return this.dbId;
	}

	public long getSeqNo() {
		return nextSeqNo;
	}

	public String dbIdToString() {
		if (group.isPrimary()) {
			return "primary db, id: " + dbId;
		} else {
			return "backup db, id: " + dbId;
		}
	}

	private void executeTransactionsSql(ShadowTransaction t, Connection connection, long now)
		throws SQLException {

		if (t instanceof BatchShadowT) {
			BatchShadowT batch = (BatchShadowT) t;
			for (ShadowTransaction trans : batch.getBatch()) {
				if (group.isPrimary()) {
					QueryResult result = trans.executeSql(connection);

					TRANS_EXECUTED.put(trans.getId(), result);
					toGC.put(now, trans.getId());

				} else if (!trans.isReadOnly()){
					trans.executeSql(connection);
				}
			}
		} else {
			if (group.isPrimary() || !t.isReadOnly()) {
				QueryResult result = t.executeSql(connection);

				TRANS_EXECUTED.put(t.getId(), result);
				toGC.put(now, t.getId());
			}
		}
	}

	private void executeTransactions2Phase(ShadowTransaction t, Connection connection, long now) throws SQLException {

		if (t instanceof BatchShadowT) {
			BatchShadowT batch = (BatchShadowT) t;

			for (ShadowTransaction trans : batch.getBatch()) {
				if (group.isPrimary()) {
					QueryResult result = trans.executeQuery(connection);

					if (!trans.isReadOnly()) {
						trans.executeUpdate(connection);
					}

					TRANS_EXECUTED.put(trans.getId(), result);
					toGC.put(now, trans.getId());
				} else if (!trans.isReadOnly()){
					trans.executeUpdate(connection);
				}
			}
		} else {
			if (group.isPrimary() || !t.isReadOnly()) {
				QueryResult result = t.executeSql(connection);

				TRANS_EXECUTED.put(t.getId(), result);
				toGC.put(now, t.getId());
			}
		}
	}

	/**
	 * This method is invoked by the primary to set the sequence number,
	 * incarnation number, and last committed transaction before sending it
	 * to the backups.
	 */
	private void setTransactionFields(ShadowTransaction t) {
		t.setSeqNo(nextSeqNo);
		t.setGroupIncarnation(group.getIncarnation());
	}

	/****** The methods below are callbacks invoked by the group ******/

	public void onNewConnection(FailFastSocket clientSocket, Selector selector,
		boolean isPrimary) {

		if (isPrimary) {
			ClientHandler handler = new ClientHandler(clientSocket, selector);
			handler.start();
		}
	}

	public void onMsgFromBackup(FailFastSocket backupSocket, long now) throws Exception {
		TransactionId id = (TransactionId) backupSocket.readObject();

		if (!acknowledgments.containsKey(id)) {
			acknowledgments.put(id, new HashSet<String>());
		} else if (acknowledgments.get(id).contains(backupSocket.getEndPointId())) {
			LOG.warning(dbIdToString() + " already received acknowledgment from "
				+ backupSocket.getEndPointId() + " for id " + id);
			return;
		}

		acknowledgments.get(id).add(backupSocket.getEndPointId());

		if (acknowledgments.get(id).size() == (group.getGroupSize() - 1)) {
			acknowledgments.remove(id);

			LinkedList<TransactionId> transIds;

			if (id instanceof BatchId) {
				transIds = ((BatchId) id).getIds();
			} else {
				transIds = new LinkedList<TransactionId>();
				transIds.add(id);
			}

			for (TransactionId transId : transIds) {
				TransactionId clientTId = id2IdMap.get(transId);

				/**
				 * The id is not in the id2IdMap for acknowledgments
				 * related to a DbSchema transaction (sent when a reconfiguration occurs).
				 */
				if (clientTId != null) {
					synchronized (clientTId) {
						clientTId.notify();
					}
					id2IdMap.remove(transId);
				}
			}
		}		
	}

	
	public void onGroupReconfiguration(long seqNo) throws Exception {

		nextSeqNo = seqNo;

		if (group.isPrimary()) {

			// Send the database schema.
			DbSchema dbSchema = new DbSchema(idGenerator.getNextId(), connection);
			setTransactionFields(dbSchema);
			group.sendToBackups(dbSchema);

			LOG.info(dbIdToString() + " sent DB schema to backups: " + dbSchema);

			ResultSet rowSet;

			// Send the table rows.
			for (String table : dbSchema.getTables().keySet()) {
				RowBatch rows;
				boolean firstBatch = true;

				/**
				 * Compute how many rows per batch to send such that
				 * a batch of rows takes not more than the maximum allowed
				 * message size in serialized form.
				 */
				int batchSize = ShadowDBConfig.getMaxMsgSize() /
					(ShadowDBConfig.getMaxDbCellSize() * dbSchema.getTables().get(table).size());

				Statement stmt = connection.createStatement();
				rowSet = stmt.executeQuery("select * from " + table);

				do {
					rows = new RowBatch(idGenerator.getNextId(), table,
						dbSchema.getTables().get(table), firstBatch, rowSet, batchSize);
					firstBatch = false;

					setTransactionFields(rows);
					group.sendToBackups(rows);

				} while (!rows.lastTableRows());

				rowSet.close();
				stmt.close();
			}

			connection.commit();
			nextSeqNo++;
		}
	}

	public void onTransactionFromPrimary(FailFastSocket socket, long now) throws Exception {

		if (!socket.getEndPointId().equals(group.getPrimaryId())) {
			LOG.warning(dbIdToString() + " received transaction from non-primary server");
			return;
		}

		ShadowTransaction t = (ShadowTransaction) socket.readObject();

		if (t.getSeqNo() == nextSeqNo && t.getGroupIncarnation() == group.getIncarnation()) {

			/**
			 *  If this is a snapshot, apply the snapshot to the local DB.
			 */
			if (t instanceof DbSchema) {
				DbSchema schema = (DbSchema) t;

				LOG.info("Received schema: " + schema);

				// Create the database schema.
				schema.executeSql(connection);
				connection.commit();

				// Populate each table
				for (int i = 0; i < schema.getTables().size(); i++) {
					RowBatch rows;

					do {
						rows = (RowBatch) group.receiveFromPrimary();
						rows.executeSql(connection);

					} while (!rows.lastTableRows());
				}

				// Swap the received snapshot with our database.
				schema.installSchema(connection, dbType);

			} else {
				if (transMode == ShadowDBConfig.TRANS_MODE_TYPE.TWO_PHASE) {
					executeTransactions2Phase(t, connection, now);
				} else {
					executeTransactionsSql(t, connection, now);
				}
				connection.commit();
			}

			group.sendToPrimary(t.getId());
			gcTransactions(now);
			nextSeqNo++;

		} else {
			LOG.severe(dbIdToString() + " received transaction with seqNo: " + t.getSeqNo() +
				" and group incarnation: " + t.getGroupIncarnation() + " local seqNo: "
				+ nextSeqNo + " group incarnation: " + group.getIncarnation());
		}
	}

	public void executeTransactionsAtPrimary(long now) throws Exception {

		ShadowTransaction t = (ShadowTransaction) TRANS_TO_EXECUTE.poll();

		if (t != null) {
			BatchShadowT batch = BatchShadowT.newInstance(t);
			id2IdMap.put(t.getId(), t.getId());

			while ((t = TRANS_TO_EXECUTE.poll()) != null) {
				batch.add(t);
				id2IdMap.put(t.getId(), t.getId());
			}

			if (transMode == ShadowDBConfig.TRANS_MODE_TYPE.TWO_PHASE) {
				executeTransactions2Phase(batch, connection, now);
			} else {
				executeTransactionsSql(batch, connection, now);
			}
			connection.commit();

			if (replicated) {
				setTransactionFields(batch);
				group.sendToBackups(batch);
				nextSeqNo++;
			} else {
				for (ShadowTransaction trans : batch.getBatch()) {
					TransactionId transId = trans.getId();

					synchronized (id2IdMap.get(transId)) {
						id2IdMap.get(transId).notify();
					}
					id2IdMap.remove(transId);
				}
			}
		}
		gcTransactions(now);
	}

	/**
	 * Garbage collect all transactions from TRANS_EXECUTED that are more
	 * than gcTimeoutMillis milliseconds old.
	 */
	public void gcTransactions(long now) {

		HashSet<Long> toDelete = new HashSet<Long>();

		// Iteration order follows the timestamp order.
		for (long ts : toGC.keySet()) {
			if (ts < (now - gcTimeoutMillis)) {
				toDelete.add(ts);
			} else {
				break;
			}
		}

		for (long ts : toDelete) {
			TransactionId id = toGC.get(ts);
			TRANS_EXECUTED.remove(id);
			toGC.remove(ts);
		}
	}
	/*********************** End of callbacks ************************/

	public static void main(String[] args) {
		if (args.length == 3) {
			ShadowDBServer dbServer = null;

			String dbId = args[0];
			String configFile = args[1];
			boolean replicated = args[2].equals("replicated");

			LinkedList<IdIpPort> allDbs = null;
			LinkedList<IdIpPort> tobcastServers = null;
			int dbPort;
			int clientPort;

			try {
				ConfigurationParser configParser = new ConfigurationParser(configFile);
				dbPort = configParser.getPortFromId(dbId);

				allDbs = configParser.getDbServers();
				tobcastServers = configParser.getToBCastServers();
				clientPort = configParser.getClientPort(dbId);
				DB_TYPE dbType = configParser.getDbType(dbId);
				configParser.closeConfigFile();

				int nextSeqNo = 1;

				dbServer = new ShadowDBServer(dbId, dbPort, replicated, dbType,
					nextSeqNo, allDbs, tobcastServers, clientPort);
				ShutdownHook.addDbToClose(dbServer);
				ShutdownHook.installHook();

				// Start the group thread.
				dbServer.group.start();
			} catch(Exception e) {
				LOG.severe("Unable to start shadowDb properly: " + e);
				return;
			}
			
			try {
				// Both the primary and backups join on the group thread.
				dbServer.group.join();
			} catch (Exception e) {
				LOG.info("Exception: " + e + " caught when waiting on group thread to terminate");
			}
		} else {
			System.err.println("Please specify the db id, the configuration file, "
				+ " and whether the db is replicated or stdalone (replicated/stdalone)");
		}
	}

	private static class ClientHandler extends Thread {

		private FailFastSocket clientSocket;
		private Selector selector;
	
		public ClientHandler(FailFastSocket clientSocket, Selector selector) {
			this.clientSocket = clientSocket;
			this.selector = selector;
		}

		public void run() {
			try {
				for (;;) {
					ShadowTransaction t = (ShadowTransaction) clientSocket.readObject();

					QueryResult result = TRANS_EXECUTED.get(t.getId());

					// Has the transaction already been executed?
					if (result == null) {
						TRANS_TO_EXECUTE.add(t);
						selector.wakeup();

						synchronized(t.getId()) {
							while (!TRANS_EXECUTED.containsKey(t.getId())) {
								t.getId().wait();
							}
						}
						result = TRANS_EXECUTED.get(t.getId());
					}

					clientSocket.writeObject(result);
				}
			} catch (Exception e) {
				LOG.fine("Client handler caught exception: " + e + " stopping thread.");
			}
		}
	}
}