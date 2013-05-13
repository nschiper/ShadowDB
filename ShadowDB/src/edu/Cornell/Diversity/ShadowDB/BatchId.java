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
 *  o File name:   BatchId.java
 *  o Description: The identifier of a batch of transactions.
 */

package edu.Cornell.Diversity.ShadowDB;

import java.io.Serializable;
import java.util.LinkedList;

/**
 * The identifier of a batch of transactions.
 * 
 * @author nschiper@.cs.cornell.edu
 */
public class BatchId extends TransactionId implements Serializable {

	private static final long serialVersionUID = 1L;

	private LinkedList<TransactionId> transactionIds;

	public BatchId(TransactionId primaryId, LinkedList<TransactionId> secondaryIds) {
		super(primaryId.getIp(), primaryId.getThreadId(), primaryId.getSeqNo());
		this.transactionIds = secondaryIds;
	}

	public static BatchId newInstance(LinkedList<ShadowTransaction> batch) {
		LinkedList<TransactionId> ids = new LinkedList<TransactionId>();
		for (ShadowTransaction t : batch) {
			ids.add(t.getId());
		}
		// The main id of the batch is simply the id of the first transaction
		return new BatchId(ids.getFirst(), ids);
	}

	public void add(TransactionId id) {
		transactionIds.add(id);
	}

	public LinkedList<TransactionId> getIds() {
		return this.transactionIds;
	}

	public boolean equals(Object o) {
		if (o instanceof BatchId) {
			LinkedList<TransactionId> ids = ((BatchId) o).getIds();

			if (transactionIds.size() != ids.size()) {
				return false;
			}
			for (int i = 0; i < transactionIds.size(); i++) {
				if (!transactionIds.get(i).equals(ids.get(i))) {
					return false;
				}
			}
			return true;
		}
		return false;
	}
}
