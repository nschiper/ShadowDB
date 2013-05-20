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
 *  o File name:   DbSchema.java
 *  o Description: A class representing the schema of a database.
 */

package edu.Cornell.Diversity.ShadowDB;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.logging.Logger;

import edu.Cornell.Diversity.Utils.DbUtils;
import edu.Cornell.Diversity.Utils.DbUtils.DB_TYPE;

/**
 * An instance of this class represents a database schema as a map of table names to the description
 * of a table. Such a description consists in a list of <columnName, type> tuples. A DbSchema object
 * also stores the primary keys of the database tables.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class DbSchema extends ShadowTransaction {

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = Logger.getLogger("edu.Cornell.Diversity.ShadowDB.DbSchema");

	private HashMap<String, LinkedHashMap<String, String>> tables;

	/**
	 * The precision of columns of variable length.
	 */
	private HashMap<String, LinkedHashMap<String, Integer>> precision;

	/**
	 * The set of primary keys.
	 */
	private HashMap<String, HashSet<String>> primaryKeys;

	public DbSchema(TransactionId id, Connection connection) throws SQLException {
		super(id, false /* read-only */);

		tables = new HashMap<String, LinkedHashMap<String,String>>();
		precision = new HashMap<String, LinkedHashMap<String, Integer>>();
		primaryKeys = new HashMap<String, HashSet<String>>();

		DatabaseMetaData metaData = connection.getMetaData();

		if (metaData != null) {
			// For each table of the db, send the rows in batches.
			String[] tablesTypes = {"TABLE"};
			ResultSet tableSet = metaData.getTables(null, null, null, tablesTypes);
			
			while (tableSet.next()) {
				String table = tableSet.getString("TABLE_NAME");
				tables.put(table, new LinkedHashMap<String, String>());
				primaryKeys.put(table, new HashSet<String>());
				
				ResultSet columnSet = metaData.getColumns(null, null, table, null);

				while (columnSet.next()) {
					String column = columnSet.getString("COLUMN_NAME");
					String columnType = columnSet.getString("TYPE_NAME");
					tables.get(table).put(column, columnType);

					// Store the precisions of VARCHAR, CHAR, and DECIMAL columns.
					if ((columnSet.getInt("DATA_TYPE") == Types.VARCHAR ||
						 columnSet.getInt("DATA_TYPE") == Types.CHAR ||
						 columnSet.getInt("DATA_TYPE") == Types.DECIMAL) &&
						 columnSet.getInt("COLUMN_SIZE") != 0) {

						if (precision.get(table) == null) {
							precision.put(table, new LinkedHashMap<String, Integer>());
						}
						precision.get(table).put(column, columnSet.getInt("COLUMN_SIZE"));

					}
				}

				ResultSet primaryKeySet = metaData.getPrimaryKeys(null, null, table);
				while (primaryKeySet.next()) {
					String column = primaryKeySet.getString("COLUMN_NAME");
					primaryKeys.get(table).add(column);
				}
			}

		} else {
			LOG.warning("Database does not provide database metadata.");
		}
	}

	/**
	 * Creates a SQL "Create Table" statement from the schema definition. To make the state transfer atomic
	 * even in the presence of potential primary failures, we copy the snapshot to temporary tables.
	 * The table created in this methods thus have "tmp" appended to their names.
	 */
	private String buildTableStatement(String tableName, LinkedHashMap<String, String> tableSchema) {
		StringBuffer sb = new StringBuffer();

		sb.append("create table " + tableName + "tmp");
		sb.append(" (");

		int index = 0;
		LinkedHashMap<String, Integer> precisionMap = precision.get(tableName);
		HashSet<String> keyMap = primaryKeys.get(tableName);

		for (String column : tableSchema.keySet()) {
			String type = tableSchema.get(column);
			sb.append(column + " " + type);

			// Specify the precision of the column, if any.
			if (precisionMap != null) {
				if (precisionMap.get(column) != null) {
					sb.append("(" + precisionMap.get(column) + ")");
				}
			}

			if (++index < tableSchema.size()) {
				sb.append(", ");
			}
		}

		index = 0;

		if (keyMap.size() > 0) {
			// Specify primary key
			sb.append(", PRIMARY KEY(");
			for (String columnKey : keyMap) {
				sb.append(columnKey);
	
				if (++index < keyMap.size()) {
					sb.append(", ");
				}
			}
			sb.append(")");
		}
		sb.append(")");
		return sb.toString();
	}

	/**
	 * Drop temporary tables, if any.
	 */
	public void dropTable(Connection connection, String tableName) throws SQLException {
		if (DbUtils.tableExists(tableName, connection)) {
			Statement stmt = connection.createStatement();

			stmt.executeUpdate("drop table " + tableName);
			stmt.close();
		}
	}

	/**
	 * Creates the tables represented by this schema.
	 */
	public void createTables(Connection connection) throws SQLException {
		for (String table : tables.keySet()) {
			Statement stmt = connection.createStatement();

			String createTable = buildTableStatement(table, tables.get(table));
			stmt.executeUpdate(createTable);
			stmt.close();
		}
	}

	/**
	 * This method renames the previously created tables to their original names.
	 * In other words, this method removes the "tmp" extensions.
	 */
	public void installSchema(Connection connection, DB_TYPE dbType) throws SQLException {
		for (String table : tables.keySet()) {
			Statement stmt = connection.createStatement();

			// Drop table, if already existing
			dropTable(connection, table);

			String renameTable;
	
			if (dbType == DB_TYPE.DERBY) {
				renameTable = "rename table " + table + "tmp to " + table;
			} else {
				renameTable = "alter table " + table + "tmp rename to " + table;
			}
			stmt.executeUpdate(renameTable);
			stmt.close();
		}
	}

	@Override
	public QueryResult executeSql(Connection connection) throws SQLException {
		// Drop temporary tables
		for (String table : tables.keySet()) {
			dropTable(connection, table + "tmp");
		}

		createTables(connection);
		return new QueryResult();
	}

	public HashMap<String, LinkedHashMap<String, String>> getTables() {
		return tables;
	}

	public HashSet<String> getPrimaryKeys(String tableName) {
		return primaryKeys.get(tableName);
	}

	public String toString() {
		StringBuffer sb = new StringBuffer();

		for (String table : tables.keySet()) {
			sb.append(table + "(");

			for (String column : tables.get(table).keySet()) {
				sb.append("<" + column + ": " + tables.get(table).get(column) + ">");
			}
			sb.append(")");
		}
		return sb.toString();
	}

	/**
	 * Returns the number of bytes of the "biggest" cell of a given table.
	 */
	public int getLargestCellSize(String table) {
		int maxSize = 0;

		for (String column : tables.get(table).keySet()) {
			String columnType = tables.get(table).get(column);

			if (getSize(column, columnType, table) > maxSize) {
				maxSize = getSize(column, columnType, table);
			}
		}
		return maxSize;
	}

	/**
	 * Returns the number of bytes of a cell of a given column
	 * in a given table.
	 */
	private int getSize(String column, String columnType, String table) {
		if (columnType.equals("INTEGER")) {
			return 4;
		} else if (columnType.equals("BIGINT")) {
			return 8;
		} else if (columnType.equals("FLOAT")) {
			return 8;
		} else if (columnType.equals("DOUBLE")) {
			return 8;

		} else if (columnType.equals("CHAR")) {
			Integer columnPrecision = precision.get(table).get(column);
			if (columnPrecision != null) {
				// To be safe, we consider that each character takes 2 bytes.
				return 2 * columnPrecision;
			} else {
				return 2;
			}

		} else if (columnType.equals("BOOLEAN")) {
			return 1;
		} else if (columnType.equals("DATE")) {
			// There is at most 24 characters of 2 bytes.
			return 48;
		} else if (columnType.equals("VARCHAR")) {

			Integer columnPrecision = precision.get(table).get(column);
			if (columnPrecision != null) {
				// To be safe, we consider that each character takes 2 bytes.
				return 2 * columnPrecision;
			} else {
				return 2;
			}

		} else if (columnType.equals("TIMESTAMP")) {
			return 8;
		} else if (columnType.equals("DECIMAL")) {
			// 41 characters of 2 bytes.
			return 82;
		} else if (columnType.equals("CHARACTER")) {

			Integer columnPrecision = precision.get(table).get(column);
			if (columnPrecision != null) {
				// To be safe, we consider that each character takes 2 bytes.
				return 2 * columnPrecision;
			} else {
				return 2;
			}

		} else {
			throw new IllegalArgumentException("Unsupported column type: " + columnType);
		}
	}
}
