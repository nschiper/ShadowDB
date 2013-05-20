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
 *  o File name:   ShutdownHook.java
 *  o Description: A utility class that ensures that TCP and database connections are closed
 *  			   when the application is terminated.
 */

package edu.Cornell.Diversity.Utils;

import java.net.ServerSocket;
import java.util.LinkedList;

import edu.Cornell.Diversity.ShadowDB.ShadowDBServer;

/**
 * A utility class that ensures that TCP and database connections are closed
 * when the application is terminated.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class ShutdownHook {

	private static LinkedList<ServerSocket> socketsToClose = new LinkedList<ServerSocket>();

	private static LinkedList<ShadowDBServer> dbsToClose = new LinkedList<ShadowDBServer>();

	public static void addSocketToClose(ServerSocket socket) {
		socketsToClose.add(socket);
	}

	public static void addDbToClose(ShadowDBServer db) {
		dbsToClose.add(db);
	}

	/**
	 * Installs a shutdown hook that properly closes the registered
	 * sockets and databases.
	 */
	public static void installHook() {

		Runtime.getRuntime().addShutdownHook(new Thread()
        {
            @Override
            public void run()
            {
                try {
                	for (ServerSocket socket : socketsToClose) {
	                	if (socket != null) {
	                		socket.close();
	                	}
                	}
 
                	for (ShadowDBServer db : dbsToClose) {
                		if (db != null) {
                			db.closeDb();
                		}
                	}
                } catch (Exception e) {
                	// There's not much we can do here...
                }
            }
        });
	}
}
