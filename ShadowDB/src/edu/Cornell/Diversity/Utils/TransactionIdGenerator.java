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
 *  o File name:   TransactionIdGenerator.java
 *  o Description: A utility class to generate transaction identifiers.
 */

package edu.Cornell.Diversity.Utils;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.logging.Logger;

import edu.Cornell.Diversity.ShadowDB.TransactionId;

/**
 * This is a helper class that can be used to generate
 * unique transaction ids.
 * 
 * @author nschiper@.cs.cornell.edu
 */
public class TransactionIdGenerator {

	private static final Logger LOG = Logger.getLogger("edu.Cornell.Diversity.ShadowDB.TransactionIdGenerator");

	private String ip;
	private long threadId;
	private int seqNo;

	public TransactionIdGenerator() {
		try {
			this.ip = InetAddress.getLocalHost().toString() + "@" + System.currentTimeMillis();
			this.threadId = Thread.currentThread().getId();
			this.seqNo = 0;
		} catch (UnknownHostException e) {
			LOG.severe("Unable to instantiate object of class TransactionIdGenerator: " + e);
		}
	}

	public TransactionId getNextId() {
		return new TransactionId(ip, threadId, ++seqNo);
	}
}
