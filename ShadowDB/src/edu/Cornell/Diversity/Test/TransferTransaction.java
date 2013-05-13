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
 *  o File name:   TransferTransaction.java
 *  o Description: A money transfer transaction.
 */

package edu.Cornell.Diversity.Test;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import edu.Cornell.Diversity.ShadowDB.QueryResult;
import edu.Cornell.Diversity.ShadowDB.ShadowTransaction;
import edu.Cornell.Diversity.ShadowDB.TransactionId;
import edu.Cornell.Diversity.Utils.DbUtils;

/**
 * This class represents a transfer transaction of a certain
 * amount from a source account to a destination account.
 * 
 * @author nschiper@.cs.cornell.edu
 */
public class TransferTransaction extends ShadowTransaction {

	private static final long serialVersionUID = 1L;

	private transient static PreparedStatement sqlUpdateStatement = null;
	private static final String sqlUpdate = "update " + BankingApp.TABLE_NAME +
		" set BALANCE = (BALANCE + ?) where ACCOUNT_ID = ?";
	private transient static PreparedStatement sqlQueryStatement = null;
	private static final String sqlQuery = "select ACCOUNT_ID, BALANCE from "
		+ BankingApp.TABLE_NAME + " where ACCOUNT_ID = ?";

	private int srcAccount;
	private int destAccount;

	int amount;

	public TransferTransaction(TransactionId id, int srcAccount, int destAccount, int amount) {
		super(id, false /* read-only? */);
		this.srcAccount = srcAccount;
		this.destAccount = destAccount;

		this.amount = amount;
	}

	public QueryResult executeSql(Connection connection) throws SQLException {

		QueryResult queryResult = new QueryResult();

		if (DbUtils.statementNullOrStale(sqlUpdateStatement, connection)) {
			sqlUpdateStatement = connection.prepareStatement(sqlUpdate);
		}
		if (DbUtils.statementNullOrStale(sqlQueryStatement, connection)) {
			sqlQueryStatement = connection.prepareStatement(sqlQuery);			
		}

		sqlUpdateStatement.setFloat(1, (float) -amount);
		sqlUpdateStatement.setInt(2, srcAccount);
		sqlQueryStatement.setInt(1, srcAccount);
		ResultSet result = sqlQueryStatement.executeQuery();

		if (result.next() && result.getFloat("BALANCE") >= amount) {
			if (sqlUpdateStatement.executeUpdate() == 1) {
				sqlUpdateStatement.setFloat(1, (float) +amount);
				sqlUpdateStatement.setInt(2, destAccount);
				if (sqlUpdateStatement.executeUpdate() == 1) {
					sqlQueryStatement.setInt(1, srcAccount);
					ResultSet resultSet = sqlQueryStatement.executeQuery();
					queryResult.addResultSet(resultSet);
					resultSet.close();
					sqlQueryStatement.setInt(1, destAccount);
					resultSet = sqlQueryStatement.executeQuery();
					queryResult.addResultSet(resultSet);
					resultSet.close();
				}
			}
		}
		return queryResult;
	}

	public String toString() {
		StringBuffer sb = new StringBuffer();
		sb.append("Transfer of: " + amount);
		sb.append("$ from account: " + srcAccount);
		sb.append(" to account: " + destAccount);
		return sb.toString();
	}

	@Override
	public QueryResult executeQuery(Connection connection) throws SQLException {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean executeUpdate(Connection connection) throws SQLException {
		throw new UnsupportedOperationException();
	}
}
