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
 *  o File name:   ShadowDBConfig.java
 *  o Description: Default configuration parameters of ShadowDB.
 */

package edu.Cornell.Diversity.ShadowDB;

/**
 * The default parameters of ShadowDB.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class ShadowDBConfig {

	/**
	 * Default timeout in seconds after which transaction results are garbage collected.
	 * One must ensure that the likelihood of garbage collecting the transaction
	 * results before the client receives them is very low.
	 * Otherwise, a transaction may be executed twice when the primary database
	 * fails before sending the results to the client, and the client resubmits
	 * the transaction although backups have executed the transaction already. 
	 */
	private static final int DEFAULT_GC_TIMEOUT_MILLIS = 20000;

	public static int getGcTimeoutMillis() {
		if (System.getProperty("gcTimeoutMillis") != null) {
			return Integer.parseInt(System.getProperty("gcTimeoutMillis"));
		} else {
			return DEFAULT_GC_TIMEOUT_MILLIS;
		}
	}

	protected static enum TRANS_MODE_TYPE {NORMAL, TWO_PHASE};

	/**
	 * When the transaction mode is set to NORMAL, transactions extending class
	 * ShadowTransaction must implement executeSQL. When set to TWO_PHASE,
	 * executeQuery and executeUpdat must be implemented. The query phase is
	 * only executed on the primary, the update phase is executed both
	 * on the primary and the backups. The query phase must fully
	 * determine the update phase. This is typically accomplished
	 * using instance variables.
	 */
	private static final TRANS_MODE_TYPE DEFAULT_TRANSACTION_MODE = TRANS_MODE_TYPE.NORMAL;

	public static TRANS_MODE_TYPE getTransactionMode() {
		if (System.getProperty("transactionMode") != null) {
			return TRANS_MODE_TYPE.valueOf(System.getProperty("transactionMode"));
		} else {
			return DEFAULT_TRANSACTION_MODE;
		}
	}

	/**
	 * Timeout (in milliseconds) used when connecting to a socket,
	 * and sending/receiving messages on that socket.
	 */
	private static final int DEFAULT_SOCKET_TIMEOUT = 10000;

	public static int getSocketTimeout() {
		if (System.getProperty("socketTimeout") != null) {
			return Integer.parseInt(System.getProperty("socketTimeout"));
		} else {
			return DEFAULT_SOCKET_TIMEOUT;
		}
	}

	/**
	 * The maximum number of bytes for a message sent over
	 * the network. This can be increased if needed.
	 */
	private static final int DEFAULT_MAX_MSG_SIZE = 50000;

	public static int getMaxMsgSize() {
		if (System.getProperty("maxMsgSize") != null) {
			return Integer.parseInt(System.getProperty("maxMsgSize"));
		} else {
			return DEFAULT_MAX_MSG_SIZE;
		}
	}

	/**
	 * The default maximum size of a database cell. If this value is not large
	 * enough and it is not set to a higher value using the corresponding Java
	 * property, replicas will not be able to send database snapshots and
	 * recovery from a failure will not succeed.
	 */
	private static final int DEFAULT_MAX_DB_CELL_SIZE = 100;

	public static int getMaxDbCellSize() {
		if (System.getProperty("maxDbCellSize") != null) {
			return Integer.parseInt(System.getProperty("maxDbCellSize"));
		} else {
			return DEFAULT_MAX_DB_CELL_SIZE;
		}
	}

	/**
	 * The default number of milliseconds between the sending
	 * of two heart beats.
	 */
	private static final int DEFAULT_HEARTBEAT_INTERVAL = 1000;

	public static int getHeartBeatInterval() {
		if (System.getProperty("heartBeatInterval") != null) {
			return Integer.parseInt(System.getProperty("heartBeatInterval"));
		} else {
			return DEFAULT_HEARTBEAT_INTERVAL;
		}
	}

	/**
	 *  The default time the primary waits before connecting to the backups in
	 *  the new configuration. This is to make sure that they all
	 *  delivered the new configuration.
	 */
	private static final int DEFAULT_GROUP_RECONFIGURATION_TIME = 10000;

	public static int getGroupReconfigurationTime() {
		if (System.getProperty("groupReconfigurationTime") != null) {
			return Integer.parseInt(System.getProperty("groupReconfigurationTime"));
		} else {
			return DEFAULT_GROUP_RECONFIGURATION_TIME;
		}
	}

	/**
	 * The maximum default number of simultaneous failures tolerated.
	 */
	private static final int DEFAULT_F = 1;

	public static int getF() {
		if (System.getProperty("f") != null) {
			return Integer.parseInt(System.getProperty("f"));
		} else {
			return DEFAULT_F;
		}
	}
}
