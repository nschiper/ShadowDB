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
 *  o File name:   IdIpPort.java
 *  o Description: A class representing an (id, ip, port) triple.
 */

package edu.Cornell.Diversity.Utils;

import java.io.Serializable;
import java.util.Scanner;

/**
 * A class that represents an (id, ip, port) triple.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class IdIpPort implements Serializable {

	private static final long serialVersionUID = 1L;

	String id;
	String ip;
	int port;

	public IdIpPort(String id, String ip, int port) {
		this.id = id;
		this.ip = ip;
		this.port = port;
	}

	public String getId() {
		return id;
	}

	public String getIp() {
		return ip;
	}

	public int getPort() {
		return port;
	}

	public IpPort getIpPort() {
		return new IpPort(ip, port);
	}

	public String toString() {
		return id + "-" + ip + "-" + port;
	}

	/**
	 * Returns a string representation of an IdIpPort objects that does
	 * not contain reserved Nuprl characters.
	 */
	public String toNuPRLString() {
		StringBuffer sb = new StringBuffer();
		sb.append(id + "-");
		Scanner scanner = new Scanner(ip).useDelimiter("\\.");
		for (int i = 0; i < 4; i++) {
			sb.append(scanner.nextInt() + "-");
		}
		sb.append(port);
		return sb.toString();
	}

	public boolean equals(Object o) {
		if (o instanceof IdIpPort) {
			IdIpPort idIpPort = (IdIpPort) o;
			return id.equals(idIpPort.getId()) && ip.equals(idIpPort.getIp()) && port == idIpPort.getPort();
		} else {
			return false;
		}
	}
}
