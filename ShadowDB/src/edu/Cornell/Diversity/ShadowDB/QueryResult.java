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
 *  o File name:   QueryResult.java
 *  o Description: The result returned by the execution of a transaction.
 */

package edu.Cornell.Diversity.ShadowDB;

import java.io.Serializable;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Set;


/**
 * A class that represents the results of a query. These results
 * are stored in a LinkedHashmap<Integer, Hashmap<String, Object>>.
 * 
 * The LinkedHashmap simply orders the rows of the result.
 * Each row has a number of columns, each with a name, and each cell of the row stores an object.
 * 
 * @author nschiper@cs.cornell.edu
 *
 */
public class QueryResult implements Serializable {

	private static final long serialVersionUID = 1L;

	private boolean aborted = false;

	private final LinkedHashMap<Integer, HashMap<String, Object>> results;

	/**
	 * Stores the index from which the next rows will be stored.
	 */
	private int nextIndex = 1;

	public QueryResult() {
		this.results = new LinkedHashMap<Integer, HashMap<String,Object>>();
	}

	public QueryResult(ResultSet resultSet) throws SQLException {
		this.results = new LinkedHashMap<Integer, HashMap<String,Object>>();

		addResultSet(resultSet);
	}

	public boolean hasAborted() {
		return aborted;
	}

	/**
	 * Adds the content of this result set to the query result.
	 * Returns true iff rows were added.
	 */
	public boolean addResultSet(ResultSet resultSet) throws SQLException {
		int previousIndex = nextIndex;

		ResultSetMetaData metaData = resultSet.getMetaData();

		int columnCount = metaData.getColumnCount();
		String[] columnNames = new String[columnCount];

		for (int i = 1; i <= columnCount; i++) {
			columnNames[i-1] = metaData.getColumnName(i);
		}

		while(resultSet.next()) {
			HashMap<String, Object> row = new HashMap<String, Object>();
			for (int i = 0; i < columnNames.length; i++) {
				row.put(columnNames[i], resultSet.getObject(i+1));
			}
			results.put(nextIndex, row);
			nextIndex++;
		}
		return (previousIndex < nextIndex);
	}

	public boolean addObjects(HashMap<String, Object> objects) {
		results.put(nextIndex, objects);
		nextIndex++;
		return true;
	}

	public Set<Integer> getKeys() {
		return results.keySet();
	}

	public Object getValue(String databaseName, int key) {
		return results.get(key).get(databaseName);
	}

	public HashMap<String, Object> getRow(int rowIndex) {
		return results.get(rowIndex);
	}

	public int getRowCount() {
		return this.nextIndex-1;
	}

	public boolean isEmpty() {
		return results.size() == 0;
	}

	public String toString() {
		StringBuffer sb = new StringBuffer();

		sb.append("Query result: \n");
		if (results.isEmpty()) {
			sb.append("\t Result set empty \n");
		} else {
			for (int i = 0; i < getRowCount(); i++) {
				HashMap<String, Object> row = results.get(i+1);

				for (String column : row.keySet()) {
					sb.append("\t column: " + column + " value: " + row.get(column));
				}
				sb.append("\n");
			}
			sb.append("\n");
		}

		return sb.toString();
	}
}
