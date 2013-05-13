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
 *  o File name:   DbUtils.java
 *  o Description: A utility class to interact with a JDBC database.
 */

package edu.Cornell.Diversity.Utils;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.logging.Logger;

/**
 * A class of database-related helper methods.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class DbUtils {

	private static final Logger LOG = Logger.getLogger("edu.Cornell.Diversity.ShadowDB.DbUtils");
	public static enum DB_TYPE {
		DERBY, HSQLDB, H2, PB
	};

	public static boolean statementNullOrStale(PreparedStatement stmt, Connection connection)
		throws SQLException {

		if (stmt == null) {
			return true;
		} else if (stmt.getConnection() == null) {
			return true;
		} else if (stmt.getConnection().isClosed()) {
			return true;
		}
		return false;
	}

	public static Connection openDbSql(DB_TYPE dbType)
		throws ClassNotFoundException, SQLException {

		Connection connection = null;
		String url;

		if (dbType == DB_TYPE.DERBY) {
			Class.forName("org.apache.derby.jdbc.EmbeddedDriver");
			url = "jdbc:derby:memory:database;create=true";
			connection = DriverManager.getConnection(url, "", "");

		} else if (dbType == DB_TYPE.HSQLDB) {
			Class.forName("org.hsqldb.jdbc.JDBCDriver" );
			url = "jdbc:hsqldb:mem:database";
			connection = DriverManager.getConnection(url, "SA", "");

		} else if (dbType == DB_TYPE.H2) {
			Class.forName("org.h2.Driver");
			url = "jdbc:h2:mem:";
			connection = DriverManager.getConnection(url, "SA", "");

		} else {
			throw new IllegalArgumentException("Unsupported db type: " + dbType.name());
		}
    	connection.setAutoCommit(false);
    	connection.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE);
  
    	LOG.info("Opened connection to " + dbType + " database.");
  
    	return connection;
	}

	public static boolean tableExists(String tableName, Connection connection) throws SQLException {
		ResultSet tables = connection.getMetaData().getTables(null, null, tableName, null);

		while (tables.next()) {
			if (tables.getString("TABLE_NAME").equals(tableName)) {
				return true;
			}
		}
		return false;
	}
}
