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
 *  o File name:   ShadowTransaction.java
 *  o Description: The super class of transactions submitted to ShadowDB.
 */

package edu.Cornell.Diversity.ShadowDB;

import java.io.Serializable;
import java.sql.Connection;
import java.sql.SQLException;

/**
 * A class representing a transaction sent to ShadowDB. Subclasses must
 * implement executeSQL and executeQuery/executeUpdate. The first method
 * contains the entire code to be executed by the transaction. The last two methods
 * should be implemented if the transaction fits the two-phase model, where
 * in the first phase queries are executed, and in the second phase only updates
 * are applied to the database. The query phase must fully determine the update
 * phase. Parameters of the update phase are typically stored as instance
 * variables of the subclass.
 * 
 * Neither commit nor rollback should be called inside these methods. The replication
 * protocol is in charge of terminating the transactions.
 * 
 * @author nschiper@.cs.cornell.edu
 */
public abstract class ShadowTransaction implements Serializable {

	private static final long serialVersionUID = 1L;

	protected TransactionId id;

	private boolean readOnly;

	/**
	 * A unique sequence number assigned to this transaction. This integer
	 * represents the global sequence number at which this transaction
	 * is to be executed against the database. Hence, this sequence number totally
	 * orders the transaction execution.
	 */
	private long sequenceNo = 0;

	/**
	 * The group incarnation in which the primary sent this transactions
	 * to backups.
	 */
	private int groupIncarnation = 0;

	public ShadowTransaction(TransactionId id, boolean readOnly) {
		this.id = id;
		this.readOnly = readOnly;
	}

	public void setSeqNo(long seqNo) {
		this.sequenceNo = seqNo;
	}

	/**
	 * Returns a unique sequence number for this transaction. If it is equal to
	 * i, then this is the i-th transaction to be executed. If null is returned
	 * this means that no sequence number has been assigned to the transaction
	 * yet.
	 */
	public long getSeqNo() {
		return this.sequenceNo;
	}

	public void setGroupIncarnation(int groupIncarnation) {
		this.groupIncarnation = groupIncarnation;
	}

	public int getGroupIncarnation() {
		return this.groupIncarnation;
	}

	public TransactionId getId() {
		return this.id;
	}

	public boolean isReadOnly() {
		return this.readOnly;
	}

	/**
	 * Executes a transaction composed of SQL statements.
	 * 
	 * An implementation of this method should not call commit nor rollback on the connection.
	 */
	public abstract QueryResult executeSql(Connection connection) throws SQLException;

	/**
	 * executeQuery and executeUpdate should be implemented by two-phase transactions,
	 * where in the first phase a sequence of queries
	 * are executed that completely determine the updates to execute.
	 */
	public abstract QueryResult executeQuery(Connection connection) throws SQLException;
	public abstract boolean executeUpdate(Connection connection) throws SQLException;
}
