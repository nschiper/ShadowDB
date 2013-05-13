package edu.Cornell.Diversity.Test;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.concurrent.ConcurrentHashMap;

import edu.Cornell.Diversity.ShadowDB.QueryResult;
import edu.Cornell.Diversity.ShadowDB.ShadowTransaction;
import edu.Cornell.Diversity.ShadowDB.TransactionId;
import edu.Cornell.Diversity.Utils.DbUtils;

/**
 * This class represents a deposit/withdraw transaction.
 */
public class DepositTransaction extends ShadowTransaction {

	private static final long serialVersionUID = 1L;

	private transient static ConcurrentHashMap<Long, PreparedStatement> updateStatements =
		new ConcurrentHashMap<Long, PreparedStatement>();
	private transient static ConcurrentHashMap<Long, PreparedStatement> queryStatements =
		new ConcurrentHashMap<Long, PreparedStatement>();

	private transient static String sqlUpdate = "update "
		+ BankingApp.TABLE_NAME + " set BALANCE = BALANCE + ? where ACCOUNT_ID = ?";
	private transient static String sqlQuery = "select ACCOUNT_ID, BALANCE from "
		+ BankingApp.TABLE_NAME + " where ACCOUNT_ID = ?";

	private final int[] accounts;
	private final int amount;

	public DepositTransaction(TransactionId id, int[] accounts, int amount) {
		super(id, false /* read-only? */);
		this.accounts = new int[accounts.length];

		for (int i = 0; i < accounts.length; i++) {
			accounts[i] = accounts[i];
		}

		this.amount = amount;
	}

	public QueryResult executeSql(Connection connection) throws SQLException {

		long threadId = Thread.currentThread().getId();
		PreparedStatement updateStmt = updateStatements.get(threadId);
		PreparedStatement queryStmt = queryStatements.get(threadId);

		if (DbUtils.statementNullOrStale(updateStmt, connection)) {
			updateStmt = connection.prepareStatement(sqlUpdate);
			updateStatements.put(threadId, updateStmt);
		}
		if (DbUtils.statementNullOrStale(queryStmt, connection)) {
			queryStmt = connection.prepareStatement(sqlQuery);
			queryStatements.put(threadId, queryStmt);
		}

		QueryResult queryResult = new QueryResult();

		for (int i = 0; i < accounts.length; i++) {
			updateStmt.setFloat(1, (float) amount);
			updateStmt.setInt(2, accounts[i]);
			updateStmt.executeUpdate();
	
			queryStmt.setInt(1, accounts[i]);
			ResultSet resultSet = queryStmt.executeQuery();
			queryResult.addResultSet(resultSet);
			resultSet.close();
		}
		return queryResult;
	}

	public String toString() {
		StringBuffer sb = new StringBuffer();
		sb.append("Deposit: " + amount);
		sb.append("$ on accounts: " + accounts);
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
