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
 *  o File name:   SuspectedCrashException.java
 *  o Description: An exception raised when a process is suspected to have failed.
 */

package edu.Cornell.Diversity.ResilientTCP;

/**
 * A SuspectedCrashException is raised when a replica with a given id
 * is suspected to have crashed.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class SuspectedCrashException extends Exception {

	private static final long serialVersionUID = 1L;

	private String id;

	public SuspectedCrashException(String id) {
		super();
		this.id = id;
	}

	public String getId() {
		return this.id;
	}

	public String toString() {
		return "suspected  crash of " + id;
	}
}
