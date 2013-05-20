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
 *  o File name:   PopulateBankDBTransaction.java
 *  o Description: A transaction to populate the bank database.
 */

package edu.Cornell.Diversity.Test;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.logging.Logger;

import edu.Cornell.Diversity.ShadowDB.QueryResult;
import edu.Cornell.Diversity.ShadowDB.ShadowTransaction;
import edu.Cornell.Diversity.ShadowDB.TransactionId;
import edu.Cornell.Diversity.Utils.DbUtils;

/**
 * A transaction that populates the bank database.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class PopulateBankDBTransaction extends ShadowTransaction {

	private static final long serialVersionUID = 1L;
	private static transient final Logger LOG = Logger.getLogger("edu.Cornell.Diversity.Test.PopulateBankDBTransaction");

	private int accountCount;

	public PopulateBankDBTransaction(TransactionId id, int accountCount) {
		super(id, false /* read-only? */);
		this.accountCount = accountCount;
	}

	private void createTables(Connection connection) throws SQLException {
		Statement stmt = connection.createStatement();
		String createTable = "create table " + BankingApp.TABLE_NAME +
			" (ACCOUNT_ID INTEGER PRIMARY KEY, CUSTOMER_ID INTEGER, BALANCE FLOAT)";

		stmt.executeUpdate(createTable);
		stmt.close();
	}

	private void populateSqlDb(Connection connection) throws SQLException {
		PreparedStatement insertStmt;

		insertStmt = connection.prepareStatement("insert into " + BankingApp.TABLE_NAME +
			" values (?, ?, ?)");

		for (int i = 1; i <= accountCount; i++) {
			insertStmt.setInt(1, i);
			insertStmt.setInt(2, i);
			insertStmt.setFloat(3, 0f);
			insertStmt.addBatch();

			if (i % 1000 == 0) {
				insertStmt.executeBatch();
				insertStmt.clearBatch();
			}
		}
		// Make sure that the last batch gets executed.
		if (accountCount % 1000 != 0) {
			insertStmt.executeBatch();
			insertStmt.close();
		}
	}

	@Override
	public QueryResult executeSql(Connection connection) throws SQLException {
		if (!DbUtils.tableExists(BankingApp.TABLE_NAME, connection)) {
			createTables(connection);
			populateSqlDb(connection);
			LOG.info("Populated table " + BankingApp.TABLE_NAME + " with "
				+ accountCount + " accounts.");
		} else {
			LOG.info("Table " + BankingApp.TABLE_NAME + " already exists, skipping database population.");
		}
		return new QueryResult();
	}
}
