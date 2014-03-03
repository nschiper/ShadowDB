/* Copyright 2012-2014 Cornell University
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
 *  o File name:   ShadowDBSMRConnection.java
 *  o Description: A connection to shadowDB when replicated with state machine replication.
 */

package edu.Cornell.Diversity.ShadowDB;

import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.logging.Logger;

import edu.Cornell.Diversity.ResilientTCP.FailFastSocket;
import edu.Cornell.Diversity.ResilientTCP.SuspectedCrashException;
import edu.Cornell.Diversity.ShadowDB.QueryResult;
import edu.Cornell.Diversity.TOBroadcast.AnerisMessage;
import edu.Cornell.Diversity.TOBroadcast.TobcastClient;
import edu.Cornell.Diversity.Utils.IdIpPort;

/**
 * This class models a connection to the ShadowDBSMR servers. An instance of this class
 * is bound to a group of replicas, and allows to submit transactions.
 * Failures of any of the replicas are transparent as total order broadcast is used to disseminate
 * transactions to the replicas.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class ShadowDBSMRConnection implements DBConnection {

	private static final Logger LOG = Logger.getLogger("edu.Cornell.Diversity.Client.ShadowDBSMRConnection");

	private String clientId;
	private LinkedList<IdIpPort> allDbs;

	TobcastClient tobcastClient;

	/**
	 * Fail-fast sockets to the servers.
	 */
	private HashMap<String, FailFastSocket> servers;

	public ShadowDBSMRConnection(String clientId, LinkedList<IdIpPort> allDbs, TobcastClient tobcastClient)
		throws Exception {

		this.clientId = clientId;
		this.allDbs = allDbs;
		this.servers = new HashMap<String, FailFastSocket>();
		this.tobcastClient = tobcastClient;
		connectToServers();
	
		LOG.info("Opened connection to the ShadowDB database in state machine replication mode");
	}

	private void connectToServers() {

		for (IdIpPort db : allDbs) {
			FailFastSocket socket = null;

			try {
				socket = FailFastSocket.newInstance(db.getIp(), db.getPort(), clientId, db.getId());
				servers.put(db.getId(), socket);

			} catch (SuspectedCrashException sce) {
				LOG.info("Could not connect to database: " + db);
			}
		}
	}

	public QueryResult submit(AnerisMessage miniTransaction) {

		assert(miniTransaction.getType() == AnerisMessage.MessageType.TRANS);

		tobcastClient.toBcast(miniTransaction);
		QueryResult queryResult = null;

		// Retrieve results from all non-crashed servers.
		for (FailFastSocket socket : servers.values()) {
			if (socket.isConnected()) {
				try {
					queryResult = (QueryResult) socket.readObject();

				} catch (Exception e) {
					if (e instanceof SuspectedCrashException) {
						// If we suspect one of the databases to have crashed, close the connection.
						SuspectedCrashException sce = (SuspectedCrashException) e;
						LOG.info("\n Suspected crash of database: " + sce.getId() + "\n");
	
						try {
							socket.close();
						} catch (IOException ioe) {
							// There's not much to be done here...
						}
					} else {
						LOG.warning("Caught exception: " + e + " while submitting transaction: "
							+ miniTransaction);
					}
				}
			}
		}
		return queryResult;
	}

	public void close() throws IOException {
		for (FailFastSocket socket : servers.values()) {
			socket.close();
		}
		//tobcastClient.closeSockets();
	}

	public QueryResult submit(ShadowTransaction transaction) {
		throw new UnsupportedOperationException("When Aneris is in Lisp mode, only transactions"
			+ " expressed in the form of Aneris messages are supported");
	}
}
