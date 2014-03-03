package edu.Cornell.Diversity.Test;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.LinkedList;

import edu.Cornell.Diversity.ShadowDB.QueryResult;
import edu.Cornell.Diversity.ShadowDB.ShadowTransaction;
import edu.Cornell.Diversity.ShadowDB.TransactionId;
import edu.Cornell.Diversity.Utils.DbUtils;

public class MiniTransaction extends ShadowTransaction {

	private static final long serialVersionUID = 1L;

	private final static String CREATE_TABLE_SQL = "create table " + BankingApp.TABLE_NAME +
		"(account_id int, balance float, primary key (account_id))";

	private final static String POPULATE_SQL = "insert into " + BankingApp.TABLE_NAME + " values (?,?)";
	private final static String READ_STMT_SQL = "select balance from " + BankingApp.TABLE_NAME +
		" where account_id = ?";
	private final static String WRITE_STMT_SQL = "update " + BankingApp.TABLE_NAME + 
		" set balance = balance + ? where account_id = ?";

	private static PreparedStatement READ_STMT;
	private static PreparedStatement WRITE_STMT;

	LinkedList<Integer> keysToRead;
	HashMap<Integer, Integer> keysToWrite;

	public MiniTransaction(TransactionId id, LinkedList<Integer> keysToRead,
		HashMap<Integer, Integer> keysToWrite) {

		super(id, keysToWrite.size() == 0 /*read only? */);

		this.keysToRead = keysToRead;
		this.keysToWrite = keysToWrite;
	}

	private void initStmts(Connection conn) throws SQLException {
		if (DbUtils.statementNullOrStale(READ_STMT, conn)) {
			READ_STMT = conn.prepareStatement(READ_STMT_SQL);
		}
		if (DbUtils.statementNullOrStale(WRITE_STMT, conn)) {
			WRITE_STMT = conn.prepareStatement(WRITE_STMT_SQL);
		}
	}

	private void populateDb(Connection conn) throws SQLException {
		Statement stmt = conn.createStatement();
		stmt.executeUpdate(CREATE_TABLE_SQL);
		stmt.close();

		PreparedStatement prepStmt = conn.prepareStatement(POPULATE_SQL);
		for (int i = 1; i <= BankingApp.ACCOUNT_COUNT; i++) {
			prepStmt.setInt(1, i);
			prepStmt.setFloat(2, 0f);
			prepStmt.executeUpdate();
		}
		prepStmt.close();
	}

	@Override
	public QueryResult executeSql(Connection conn) throws SQLException {
		
		QueryResult queryResult = new QueryResult();

		if (keysToRead.size() == 0 && keysToWrite.size() == 0) {
			populateDb(conn);

		} else {
			initStmts(conn);

			for (int key : keysToRead) {
				READ_STMT.setInt(1, key);
				ResultSet rs = READ_STMT.executeQuery();
				queryResult.addResultSet(rs);
				rs.close();
			}
	
			for (int key : keysToWrite.keySet()) {
				WRITE_STMT.setInt(1, keysToWrite.get(key));
				WRITE_STMT.setInt(2, key);
				WRITE_STMT.executeUpdate();
			}
		}
		return queryResult;
	}

}
