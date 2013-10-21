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
 *  o File name:   ShadowDBConnection.java
 *  o Description: A client connection to the ShadowDB replicated database.
 */

package edu.Cornell.Diversity.ShadowDB;

import java.sql.Connection;
import java.util.LinkedList;
import java.util.logging.Logger;

import edu.Cornell.Diversity.ResilientTCP.FailFastSocket;
import edu.Cornell.Diversity.ResilientTCP.SuspectedCrashException;
import edu.Cornell.Diversity.Utils.IdIpPort;

/**
 * This class models a connection to the ShadowDB database replicated with primary-backup. An instance of this class
 * is bound to a group of replica, and allows to submit transactions.
 * If the ShadowDB primary fails, any in-flight transaction will be resubmitted to the next
 * primary automatically.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class ShadowDBConnection implements DBConnection {

	private static final Logger LOG = Logger.getLogger("edu.Cornell.Diversity.Client.ShadowDBConnection");

	private String clientId;
	private LinkedList<IdIpPort> allDbs;

	/**
	 * The index of the replica we currently believe to be primary
	 * in the list of databases.
	 */
	int indexPrimary;

	/**
	 * A fail-fast socket to the current primary.
	 */
	private FailFastSocket primarySocket;

	public ShadowDBConnection(String clientId, LinkedList<IdIpPort> allDbs)
		throws Exception {

		this.clientId = clientId;
		this.allDbs = allDbs;
		this.indexPrimary = -1;
		connectToPrimary();
	}

	private void connectToPrimary() {

		boolean connected = false;

		while (!connected) {
			try {
				indexPrimary = (indexPrimary + 1) % (allDbs.size() - 1);
				IdIpPort db = allDbs.get(indexPrimary);

				LOG.info("Connecting to primary: " + db);
				primarySocket = FailFastSocket.newInstance(db.getIp(), db.getPort(), clientId, db.getId());
				LOG.info("Connected to the ShadowDB primary: " + db.getId());
				connected = true;

			} catch (SuspectedCrashException sce) {
				// If we suspect the primary to have crashed, connect to a new one...
				try {
					if (primarySocket != null) {
						primarySocket.close();
					}
					// Give some time for the new primary to be elected
					Thread.sleep(2 * ShadowDBConfig.getGroupReconfigurationTime());
				} catch (Exception e) {
					// Do nothing...
				}
			}
		}
	}

	public QueryResult submit(ShadowTransaction t) {

		QueryResult queryResult = null;
		boolean done = false;

		while (!done) {
			try {
				primarySocket.writeObject(t);
				queryResult = (QueryResult) primarySocket.readObject();

				done = true;
			} catch (Exception e) {
				if (e instanceof SuspectedCrashException) {
					/**
					 * If we suspect the primary to have crashed, connect to a new one...
					 */
					SuspectedCrashException sce = (SuspectedCrashException) e;
					LOG.info("\n Suspected crash of primary: " + sce.getId() + "\n");

					/**
					 * Waiting until the new replica configuration is installed.
					 */
					try {
						Thread.sleep(ShadowDBConfig.getGroupReconfigurationTime());
					} catch (InterruptedException ie) {
						// There's not much to be done here...
					}
					connectToPrimary();
				} else {
					LOG.warning("Caught exception: " + e + " while submitting transaction: " + t);
				}
			}
		}
		return queryResult;
	}

	public void close() throws Exception {
		primarySocket.close();
	}

	public Connection getConnection() {
		throw new UnsupportedOperationException();
	}
}
