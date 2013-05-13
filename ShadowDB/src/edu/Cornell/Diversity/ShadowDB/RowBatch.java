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
 *  o File name:   RowBatch.java
 *  o Description: A batch of table rows. This is used to send a database snapshot when recovering from a failure.
 */

package edu.Cornell.Diversity.ShadowDB;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.LinkedHashMap;
import java.util.logging.Logger;

/**
 * This class holds a batch of rows of a given table.
 * 
 * We support the following SQL types:
 * <ul>
 *   <li> INTEGER
 *   <li> BIGINT
 *   <li> FLOAT
 *   <li> DOUBLE
 *   <li> CHAR
 *   <li> BOOLEAN
 *   <li> DATE
 *   <li> VARCHAR
 *   <li> TIMESTAMP
 *   <li> DECIMAL
 * 
 * @author nschiper@cs.cornell.edu
 */
public class RowBatch extends ShadowTransaction {

	private static final long serialVersionUID = 1L;
	private static transient final Logger LOG = Logger.getLogger("edu.Cornell.Diversity.ShadowDB.RowBatch");

	private final String tableName;

	/**
	 * The type of each table column.
	 */
	private final int[] types;

	/**
	 * A set of rows. The first index is the
	 * row number, the second index is the column number.
	 */
	private Object[][] rows;

	/**
	 * True iff these are the last rows of the table.
	 */
	private boolean lastTableRows;

	public RowBatch(TransactionId id, String tableName, LinkedHashMap<String, String> types,
		boolean firstTableRowBatch, ResultSet rowSet, int maxRowCount) throws SQLException {

		super(id, false /* read-only */);
		this.tableName = tableName;
		this.types = new int[types.size()];

		int index = 0;
		for (String type : types.values()) {
			this.types[index++] = toIntColumnType(type);
		}
		this.rows = new Object[maxRowCount][types.size()];

		// This boolean might be set to true in method addRows() later on.
		this.lastTableRows = false;

		boolean tableContainsTuples = true;
		if (firstTableRowBatch) {
			tableContainsTuples = rowSet.next();
		}

		if (tableContainsTuples) {
			addRows(rowSet, maxRowCount);
		} else {
			this.rows = new Object[0][0];
		}
	}

	public boolean isEmpty() {
		return rows.length == 0;
	}

	/**
	 * Converts a string column type to an integer representation of the type (see java.sql.Types).
	 */
	public static int toIntColumnType(String columnType) {
		if (columnType.equals("INTEGER")) {
			return Types.INTEGER;
		} else if (columnType.equals("BIGINT")) {
			return Types.BIGINT;
		} else if (columnType.equals("FLOAT")) {
			return Types.FLOAT;
		} else if (columnType.equals("DOUBLE")) {
			return Types.DOUBLE;
		} else if (columnType.equals("CHAR")) {
			return Types.CHAR;
		} else if (columnType.equals("BOOLEAN")) {
			return Types.BOOLEAN;
		} else if (columnType.equals("DATE")) {
			return Types.DATE;
		} else if (columnType.equals("VARCHAR")) {
			return Types.VARCHAR;
		} else if (columnType.equals("TIMESTAMP")) {
			return Types.TIMESTAMP;
		} else if (columnType.equals("DECIMAL")) {
			return Types.DECIMAL;
		} else if (columnType.equals("CHARACTER")) {
			return Types.CHAR;
		} else {
			throw new IllegalArgumentException("Unknown column type: " + columnType);
		}
	}

	private void addRows(ResultSet resultSet, int maxRows) throws SQLException {
		int rowCount = 0;
		boolean moreRows = false;

		do {
			for (int j = 0; j < types.length; j++) {
				rows[rowCount][j] = resultSet.getObject(j + 1);
			}
			rowCount++;
		} while ((moreRows = resultSet.next()) && rowCount < maxRows);

		// If we did not fill up the array of arrows we resize the array.
		if (rowCount < maxRows) {
			Object[][] rowsTmp = new Object[rowCount][types.length];

			for (int i = 0; i < rowCount; i++) {
				for (int j = 0; j < types.length; j++) {
					rowsTmp[i][j] = rows[i][j];
				}
			}
			rows = rowsTmp;
		}

		if (!moreRows) {
			lastTableRows = true;
		}
	}

	private PreparedStatement buildInsertStmt(Connection connection) throws SQLException {
		StringBuffer sb = new StringBuffer();

		sb.append("insert into " + tableName + "tmp values (");

		for (int i = 0; i < types.length; i++) {
			sb.append("?");
			if (i < (types.length - 1)) {
				sb.append(", ");
			}
		}
		sb.append(")");

		return connection.prepareStatement(sb.toString());
	}

	private void parametrizeStmt(PreparedStatement stmt, Object[] row)
		throws Exception {

		for (int i = 0; i < row.length; i++) {
			switch (types[i]) {
			case Types.INTEGER :
				stmt.setInt(i + 1, (row[i] == null ? 0 : (Integer) row[i]));
				break;

			case Types.BIGINT :
				stmt.setLong(i + 1, (row[i] == null ? 0l : (Long) row[i]));
				break;

			case Types.FLOAT :
				stmt.setFloat(i + 1, (row[i] == null ? 0f : (Float) row[i]));
				break;
			
			case Types.DOUBLE :
				stmt.setDouble(i + 1, (row[i] == null ? 0 : (Double) row[i]));
				break;

			case Types.CHAR :
				stmt.setString(i + 1, (row[i] == null ? "" : (String) row[i]));
				break;

			case Types.BOOLEAN :
				stmt.setBoolean(i + 1, (row[i] == null ? false : (Boolean) row[i]));
				break;

			case Types.DATE :
				stmt.setDate(i + 1, (row[i] == null ? new Date(0) : (Date) row[i]));
				break;

			case Types.VARCHAR :
				stmt.setString(i + 1, (row[i] == null ? "" : (String) row[i]));
				break;

			case Types.TIMESTAMP :
				stmt.setTimestamp(i + 1, (row[i] == null ? new Timestamp(0) : (Timestamp) row[i]));
				break;

			case Types.DECIMAL :
				stmt.setBigDecimal(i + 1, (row[i] == null ? new BigDecimal(0) : (BigDecimal) row[i]));
				break;

			default :
				LOG.warning("Unsupported type: " + types[i]);
			}
		}
	}

	public void setLastTableRows() {
		lastTableRows = true;
	}

	public boolean lastTableRows() {
		return lastTableRows;
	}

	@Override
	public QueryResult executeSql(Connection connection) throws SQLException {
		PreparedStatement stmt = buildInsertStmt(connection);

		try {
			for (int i = 0; i < rows.length; i++) {
				parametrizeStmt(stmt, rows[i]);
				if (stmt.executeUpdate() != 1) {
					LOG.warning("Unable to insert tuple in database!");
				}
			}
		} catch (Exception e) {
			LOG.warning("Caught exception: " + e + " while executing RowBatch transaction: " + super.id);
			e.printStackTrace();
		}
		return new QueryResult();
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
