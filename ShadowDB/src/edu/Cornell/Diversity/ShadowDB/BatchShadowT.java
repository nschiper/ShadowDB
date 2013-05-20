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
 *  o File name:   BatchShadowT.java
 *  o Description: A batch of transactions.
 */

package edu.Cornell.Diversity.ShadowDB;

import java.io.Serializable;
import java.sql.Connection;
import java.util.LinkedList;

/**
 * A batch of transactions.
 * 
 * @author nschiper@.cs.cornell.edu
 */
public class BatchShadowT extends ShadowTransaction implements Serializable {

	private static final long serialVersionUID = 1L;

	private LinkedList<ShadowTransaction> batch;

	private BatchShadowT(BatchId id, LinkedList<ShadowTransaction> batch,
		boolean containsUpdate) {

		super(id, !containsUpdate /* Is it a read-only transaction */);
		this.batch = batch;
	}

	public static BatchShadowT newInstance(ShadowTransaction t) {
		LinkedList<ShadowTransaction> batch = new LinkedList<ShadowTransaction>();
		batch.add(t);
		return new BatchShadowT(BatchId.newInstance(batch), batch, !t.isReadOnly());
	}

	public static BatchShadowT newInstance(LinkedList<ShadowTransaction> batch) {
		boolean containsUpdate = false;
		for (ShadowTransaction t : batch) {
			if (!t.isReadOnly()) {
				containsUpdate = true;
				break;
			}
		}
		BatchId batchId = BatchId.newInstance(batch);
		return new BatchShadowT(batchId, batch, containsUpdate);
	}

	public void add(ShadowTransaction t) {
		batch.add(t);
		((BatchId) getId()).add(t.getId());
	}

	public QueryResult executeSql(Connection connection) {
		throw new UnsupportedOperationException();
	}

	public LinkedList<ShadowTransaction> getBatch() {
		return this.batch;
	}
}
