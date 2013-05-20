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
 *  o File name:   BalanceTransaction.java
 *  o Description: A transaction retrieving the balance of a bank account.
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
 * This class represents a transaction that retrives the balance of
 * a set of accounts.
 * 
 * @author nschiper@.cs.cornell.edu
 * 
 */
public class BalanceTransaction extends ShadowTransaction {

	private static final long serialVersionUID = 1L;

	private transient static final String sqlQuery = "select ACCOUNT_ID, BALANCE from "
		+ BankingApp.TABLE_NAME + " where ACCOUNT_ID = ?";
	private transient static PreparedStatement sqlStatement = null;

	int[] accounts;

	public BalanceTransaction(TransactionId id, int[] accounts) {
		super(id, true  /* read-only? */);
		this.accounts = new int[accounts.length];

		for (int i = 0; i < accounts.length; i++) {
			this.accounts[i] = accounts[i];
		}
	}

	public QueryResult executeSql(Connection connection) throws SQLException {
		if (DbUtils.statementNullOrStale(sqlStatement, connection)) {
			sqlStatement = connection.prepareStatement(sqlQuery);
		}

		QueryResult queryResult = new QueryResult();
		for (int i = 0; i < accounts.length; i++) {
			sqlStatement.setInt(1, accounts[i]);
			ResultSet resultSet = sqlStatement.executeQuery();
			
			queryResult.addResultSet(resultSet);
			resultSet.close();
		}
		return queryResult;
	}

	public String toString() {
		StringBuffer sb = new StringBuffer();
		sb.append("Get balance of accounts: (");
		for (int i = 0; i < accounts.length; i++) {
			sb.append(accounts[i]);
			if (i < (accounts.length - 1)) {
				sb.append(", ");
			}
		}
		sb.append(")");
		return sb.toString();
	}
}
