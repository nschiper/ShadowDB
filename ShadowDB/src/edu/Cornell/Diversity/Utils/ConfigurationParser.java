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
 *  o File name:   ConfigurationParser.java
 *  o Description: A class to parse the ShadowDB configuration file.
 */

package edu.Cornell.Diversity.Utils;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedList;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import edu.Cornell.Diversity.Utils.DbUtils.DB_TYPE;

/**
 * This class parses a configuration file for the database servers, and servers related to Aneris,
 * the diversified atomic broadcast service.
 * 
 * The format is the following where id is a string:
 * 
 * %locations acceptors     // These are processes running the acceptors of the Paxos protocol
 * id : ip port
 * ...
 *
 * %locations leaders       // These are processes running the leaders of the Paxos protocol
 * id : ip port
 * ...
 * 
 * %locations two_thirds	// These are processes running two-third Consensus
 * id : ip port
 * ...
 * 
 * %locations replicas   	// These are processes running the replicas of Aneris
 * id : ip port
 * ...
 * 
 * %locations external clients	// These are processes that will interact with Aneris
 * id : ip port
 * ...
 * 
 * %connections              // Describes how processes are inter-connected
 * acceptors -> leaders 
 * ...
 * 
 * %parameters
 * ...
 * 
 * %messages
 * ...
 * 
 * %databases              // These are the ShadowDB replicas
 * id : ip port
 * ...
 * 
 * @author nschiper@cs.cornell.edu
 */
public class ConfigurationParser {

	private static final Logger LOG = Logger.getLogger("edu.Cornell.Diversity.Utils.ConfigurationParser");

	// The maximum number of characters in a configuration file.
	// This is used to reset the configuration file to the beginning.
	private static final int READ_AHEAD_LIMIT = 10000;

	private static Pattern chars2Skip = Pattern.compile("\\s+|\\.|\\:");

	private BufferedReader configFile;

	private static LinkedList<String> removeEmptyTokens(String[] tokens) {
		LinkedList<String> filteredTokens = new LinkedList<String>();

		for (int i = 0; i < tokens.length; i++) {
			if (!tokens[i].equals("")) {
				filteredTokens.add(tokens[i]);
			}
		}
		return filteredTokens;
	}

	private static String getId(String configLine) {
		LinkedList<String> tokens = removeEmptyTokens(chars2Skip.split(configLine));

		if (tokens.size() == 6 || tokens.size() == 7) {
			return tokens.getFirst();
		} else {
			return null;
		}
	}

	private static String getIp(String configLine) {
		LinkedList<String> tokens = removeEmptyTokens(chars2Skip.split(configLine));
		if (tokens.size() == 6 || tokens.size() == 7) {
			return tokens.get(1) + "." + tokens.get(2) + "." + tokens.get(3) + "." + tokens.get(4);
		} else {
			return null;
		}
	}

	private static Integer getPort(String configLine) {
		LinkedList<String> tokens = removeEmptyTokens(chars2Skip.split(configLine));
		if (tokens.size() == 6 || tokens.size() == 7) {
			return Integer.parseInt(tokens.get(5));
		} else {
			return null;
		}
	}

	public DB_TYPE getDbType(String dbId) throws IOException {
		String configLine = getConfigLine(dbId);
		LinkedList<String> tokens = removeEmptyTokens(chars2Skip.split(configLine));
		if (tokens.size() == 7) {
			return DbUtils.DB_TYPE.valueOf(tokens.get(6).toUpperCase());
		} else {
			return null;
		}
	}

	public ConfigurationParser(String fileName) throws Exception {
		this.configFile = new BufferedReader(new FileReader(fileName));
		this.configFile.mark(READ_AHEAD_LIMIT);
	}

	public void closeConfigFile() throws IOException {
		configFile.close();
	}

	private String getConfigLine(String id) throws IOException {

		configFile.reset();

		// Go to the right section of the configuration
		while(configFile.ready()) {
			String line = configFile.readLine();

			if (id.equals(getId(line))) {
				return line;
			}
		}
		return null;
	}

	/**
	 * Returns the port for the given id, or null if the
	 * port cannot be retrieved from the configuration file.
	 */
	public Integer getPortFromId(String id) {
		try {
			String line = getConfigLine(id);

			if (line != null) {
				return getPort(line);
			}
		} catch (Exception e) {
			LOG.warning("Unable to find port for id: " + id + ": " + e.getMessage());
		}
		return null;
	}

	/**
	 * Returns the client port of the given db id.
	 */
	public Integer getClientPort(String dbId) {

		String clientId = "client" + DbUtils.extractIntFromId(dbId);
		return getPortFromId(clientId);
	}

	/**
	 * Returns a list of all database servers.
	 */
	public LinkedList<IdIpPort> getDbServers() {

		LinkedList<IdIpPort> servers = new LinkedList<IdIpPort>();

		try {
			configFile.reset();

			// Go to the part related to databases
			while(configFile.ready()) {
				if (configFile.readLine().contains("%databases")) {
					break;
				}
			}

			while(configFile.ready()) {
				String line = configFile.readLine();

				String id = getId(line);
				String ip = getIp(line);
				Integer port = getPort(line);
				if (id != null && ip != null && port != null) {
					servers.add(new IdIpPort(id, ip, port));
				}				
			}
		} catch (Exception e) {
			LOG.warning("Unable to find database servers: " + e);
		}
		return servers;
	}

	/**
	 * Returns a list of all clients, the processes receiving the ordered messages.
	 */
	public LinkedList<IdIpPort> getClients() {

		LinkedList<IdIpPort> servers = new LinkedList<IdIpPort>();

		try {
			configFile.reset();

			// Go to the part related to clients
			while(configFile.ready()) {
				if (configFile.readLine().contains("%locations external clients")) {
					break;
				}
			}

			while(configFile.ready()) {
				String line = configFile.readLine();

				if (line.startsWith("%")) {
					break;
				}

				String id = getId(line);
				String ip = getIp(line);
				Integer port = getPort(line);
				if (id != null && ip != null && port != null) {
					servers.add(new IdIpPort(id, ip, port));
				}				
			}
		} catch (Exception e) {
			LOG.warning("Unable to find clients: " + e);
		}
		return servers;
	}

	/**
	 * Returns a list of all total order broadcast servers.
	 */
	public LinkedList<IdIpPort> getToBCastServers() {

		LinkedList<IdIpPort> servers = new LinkedList<IdIpPort>();

		try {
			configFile.reset();

			// Go to the part related to replicas
			while(configFile.ready()) {
				if (configFile.readLine().contains("%locations replicas")) {
					break;
				}
			}

			while(configFile.ready()) {
				String line = configFile.readLine();

				if (line.startsWith("%")) {
					break;
				}

				String id = getId(line);
				String ip = getIp(line);
				Integer port = getPort(line);
				if (id != null && ip != null && port != null) {
					servers.add(new IdIpPort(id, ip, port));
				}
			}
		} catch (Exception e) {
			LOG.warning("Unable to find the total order broadcast servers (replicas): " + e);
		}
		return servers;
	}
}
