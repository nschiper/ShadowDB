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
 *  o File name:   AnerisMessage.java
 *  o Description: A message exchanged between ShadowDB and Aneris.
 */

package edu.Cornell.Diversity.TOBroadcast;

import java.util.LinkedList;
import java.util.Scanner;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import edu.Cornell.Diversity.TOBroadcast.TobcastClient.AnerisType;
import edu.Cornell.Diversity.Utils.DbUtils;
import edu.Cornell.Diversity.Utils.IdIpPort;

/**
 * This class represents messages exchanged with the Aneris total
 * ordered broadcast service.
 * 
 * An Aneris message is either a group reconfiguration request or a protocol change.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class AnerisMessage {

	private static Pattern MSG_DELIMITER_PATTERN = Pattern.compile(TobcastClient.MSG_SEPARATOR);
	private static Pattern CMD_PATTERN = Pattern.compile("\\#\\#\\#(.)+\\#\\#\\#");
	private static Pattern ID_PATTERN = Pattern.compile("\\p{Alpha}+\\d+");
	private static Pattern INTEGER_PATTERN = Pattern.compile("\\#(\\d+)\\#");
	private static Pattern PORT_PATTERN = Pattern.compile("\\-(\\d+)\\#");
	private static Pattern IP_PATTERN = Pattern.compile("\\-(\\d+\\-\\d+\\-\\d+\\-\\d+)\\-");
	private static Pattern NATURAL_NUMBER_PATTERN = Pattern.compile("natural_number:,\\d+:n");

	/**
	 * This is used to generate unique identifiers for broadcast messages.
	 */
	private static AtomicLong id = new AtomicLong(0);

	/**
	 * The proposerId of a swap message is a "FakeId0".
	 * The ids of such messages will thus start from 0.
	 */
	private static String FAKE_ID = "FakeId0";

	/**
	 * The Nuprl term format to send messages to Aneris when interpreted.
	 */
	public static final String ANERIS_SEND_MSG_FORMAT = new StringBuffer()
		.append("{pair:OPID}")
		.append("({pair:OPID}")
		.append("({token:OPID,%s:t}();")  // bcast or swap
		.append("{axiom:OPID}());")
		.append("{pair:OPID}")
		.append("({natural_number:OPID,%d:n}();") // command id
		.append("{pair:OPID}")
		.append("({token:OPID,%s:t}();") // command
		.append("{axiom:OPID}())))").toString();

	/**
	 * The Nuprl term format to send messages to Aneris when running in Lisp.
	 */
	public static final String ANERIS_LISP_SEND_MSG_FORMAT = new StringBuffer()
		.append("#S(PAIR :FST #S(PAIR :FST \"%s\"")  // bcast or swap
		.append(" :SND #S(AXIOM)) :SND #S(PAIR :FST %d")  // command id
		.append(" :SND #S(PAIR :FST \"%s\" :SND #S(AXIOM))))").toString();  // command

	/**
	 * The Aneris message format (a Nuprl term).
	 */
	public static final String ANER_RCV_MSG_FORMAT = new StringBuffer()
		.append("{pair:OPID}")
		.append("({pair:OPID}")
		.append("({token:OPID,response:t}();")
		.append("{axiom:OPID}());")
		.append("{pair:OPID}")
		.append("({natural_number:OPID,%d:n}();") //slot number
		.append("{%s:OPID}")                      // %s is the type: either inr or inl
		.append("({pair:OPID}")
		.append("({natural_number:OPID,%d:n}();") //command id
		.append("{pair:OPID}")
		.append("({token:OPID,%s:t}();") // decided command
		.append("{axiom:OPID}())))))").toString();

	public static enum ANERIS_MSG_TYPE {
		BCAST("bcast"), SWAP("swap"), DUMMY("bcast");

		private String name;

		private ANERIS_MSG_TYPE(String name) {
			this.name = name;
		}

		public String toString() {
			return name;
		}
	};

	public static enum PROTOCOL_TYPE {
		PAXOS("paxos"), TWOTHIRD("2/3");

		private String name;

		private PROTOCOL_TYPE(String name) {
			this.name = name;
		}

		public String toString() {
			return name;
		}
	}

	private ANERIS_MSG_TYPE msgType;

	private PROTOCOL_TYPE protocol;

	private long uid;

	/**
	 * Variables used for broadcast messages
	 */
	private LinkedList<IdIpPort> members;
	private long seqNo;
	private String proposerId;

	/**
	 * The index (or slot) of the Aneris message
	 */
	private long slot;

	private AnerisMessage(PROTOCOL_TYPE protocol) {
		this.msgType = ANERIS_MSG_TYPE.SWAP;
		this.protocol = protocol;
		this.proposerId = FAKE_ID;
	}

	public AnerisMessage(PROTOCOL_TYPE protocol, int proposerId) {
		this.msgType = ANERIS_MSG_TYPE.SWAP;
		this.protocol = protocol;
		this.proposerId = FAKE_ID;
		this.uid = proposerId * 100000000L + id.incrementAndGet();
	}

	public AnerisMessage(LinkedList<IdIpPort> members, long seqNo, String proposerId) {
		this.msgType = ANERIS_MSG_TYPE.BCAST;
		this.members = members;
		this.seqNo = seqNo;
		this.proposerId = proposerId;
		this.uid = DbUtils.extractIntFromId(proposerId) * 100000000L + id.incrementAndGet();
	}

	/**
	 * Builds an empty Aneris message.
	 */
	public AnerisMessage(String proposerId) {
		this.msgType = ANERIS_MSG_TYPE.DUMMY;
		this.proposerId = proposerId;
		this.uid = DbUtils.extractIntFromId(proposerId) * 100000000L + id.incrementAndGet();
	}

	public static AnerisMessage parseConfiguration(String configuration) {

		Matcher idMatcher = ID_PATTERN.matcher(configuration);
		if (idMatcher.find()) {
			String proposerId = idMatcher.group();

			Matcher seqNoMatcher = INTEGER_PATTERN.matcher(configuration);

			if (seqNoMatcher.find()) {
				Matcher ipMatcher = IP_PATTERN.matcher(configuration);
				Matcher portMatcher = PORT_PATTERN.matcher(configuration);

				int seqNo = Integer.parseInt(seqNoMatcher.group(1));
				LinkedList<IdIpPort> members = new LinkedList<IdIpPort>();

				while (idMatcher.find()) {
					String id = idMatcher.group();
					ipMatcher.find();
					String ip = ipMatcher.group(1).replace("-", ".");
					portMatcher.find();
					int port = Integer.parseInt(portMatcher.group(1));
					members.add(new IdIpPort(id, ip, port));
				}
				return new AnerisMessage(members, seqNo, proposerId);
			}
		}
		throw new IllegalArgumentException("Wrong format for group configuration: " + configuration);	
	}

	/**
	 * Parses an Aneris message when it contains a protocol.
	 */
	private static AnerisMessage parseProtocol(String msg) {
		PROTOCOL_TYPE protocol = null;
		
		if (msg.contains(PROTOCOL_TYPE.PAXOS.toString())) {
			protocol = PROTOCOL_TYPE.PAXOS;
		} else if (msg.contains(PROTOCOL_TYPE.TWOTHIRD.toString())) {
			protocol = PROTOCOL_TYPE.TWOTHIRD;
		}
		if (protocol != null) {
			AnerisMessage anerisMsg = new AnerisMessage(protocol);
			return anerisMsg;

		} else {
			throw new IllegalArgumentException("Incorrect format for new protocol: " + msg);
		}
	}

	private static AnerisMessage parseDummy(String msg) {
		Matcher idMatcher = ID_PATTERN.matcher(msg);
		if (idMatcher.find()) {
			return new AnerisMessage(idMatcher.group());
		} else {
			throw new IllegalArgumentException("Could not find proposer id for dummy message: " + msg);
		}
	}

	/**
	 * Parses one decided value in Nuprl format.
	 */
	private static AnerisMessage parseOneCmd(String msg, ANERIS_MSG_TYPE msgType) {
		AnerisMessage anerisMsg;

		if (msgType == ANERIS_MSG_TYPE.BCAST) {

			Matcher matcher = CMD_PATTERN.matcher(msg);

			if (matcher.find()) {
				String msgContent = matcher.group();

				if (msg.contains(ANERIS_MSG_TYPE.DUMMY.name())) {
					anerisMsg = parseDummy(msgContent);
				} else if (msgContent.contains(ANERIS_MSG_TYPE.BCAST.name())) {
					anerisMsg = parseConfiguration(msgContent);
				} else {
					throw new IllegalArgumentException("Unknown aneris message type: " + msg);
				}
			}  else {
				throw new IllegalArgumentException("Incorrect syntax for decided command: " + msg);
			}
		
		} else if (msgType == ANERIS_MSG_TYPE.SWAP) {
			anerisMsg = parseProtocol(msg);

		} else {
			throw new IllegalArgumentException("Unsupported message type: " + msgType);
		}

		return anerisMsg;
	}

	public static long parseSlot(String msg, AnerisType anerisType) {
		long index;

		if (anerisType == AnerisType.INTERPRETED) {
			Matcher indexMatcher = NATURAL_NUMBER_PATTERN.matcher(msg);
			while(indexMatcher.find()) {
				Scanner scanner = new Scanner(indexMatcher.group()).useDelimiter("\\p{Punct}");
				while (!scanner.hasNextLong()) {
					scanner.next();
				}
				index = scanner.nextLong();

				// The slot is the first long bigger than zero.
				if (index > 0) {
					return index;
				}
			}
		} else if (anerisType == AnerisType.LISP) {
			Scanner scanner = new Scanner(msg);
			while (scanner.hasNext()) {
				if (scanner.hasNextLong()) {
					index = scanner.nextLong();
					return index;
				} else {
					scanner.next();
				}
			}
		}
		throw new IllegalArgumentException("Unsupported aneris type: " + anerisType);
	}

	/**
	 * Parses a message when received from the interpreted version
	 * of Aneris. This message may contain multiple commands.
	 */
	private static LinkedList<AnerisMessage> parseStringInterpreted(String msg, long slot) {
		LinkedList<AnerisMessage> anerisMsgs = new LinkedList<AnerisMessage>();
		AnerisMessage anerisMsg;
	
		ANERIS_MSG_TYPE msgType;
		if (msg.contains(ANERIS_MSG_TYPE.DUMMY.name()) ||
			msg.contains(ANERIS_MSG_TYPE.BCAST.name())) {

			msgType = ANERIS_MSG_TYPE.BCAST;

		} else if (msg.contains("inl")) {
			msgType = ANERIS_MSG_TYPE.SWAP;
		} else {
			throw new IllegalArgumentException("Received aneris message has wrong format: " + msg);
		}

		// The string may contain multiple commands
		Scanner scanner = new Scanner(msg);
		scanner.useDelimiter(TobcastClient.MSG_SEPARATOR);

		while (scanner.hasNext()) {
			String cmd = scanner.next();
			anerisMsg = parseOneCmd(cmd, msgType);
			anerisMsg.setSlot(slot);
			anerisMsgs.add(anerisMsg);
		}
		return anerisMsgs;
	}

	/**
	 * Parses a message when received from the Lisp version of Aneris. This message
	 * may contain multiple commands.
	 */
	private static LinkedList<AnerisMessage> parseStringLisp(String msg, long slot) {
		LinkedList<AnerisMessage> anerisMsgs = new LinkedList<AnerisMessage>();
		AnerisMessage anerisMsg;
		ANERIS_MSG_TYPE msgType;

		if (msg.contains(ANERIS_MSG_TYPE.DUMMY.name()) ||
			msg.contains(ANERIS_MSG_TYPE.BCAST.name())) {

			msgType = ANERIS_MSG_TYPE.BCAST;

		} else {
			msgType = ANERIS_MSG_TYPE.SWAP;
		}

		// The string may contain multiple commands
		Scanner scanner = new Scanner(msg);
		scanner.useDelimiter(MSG_DELIMITER_PATTERN);

		while (scanner.hasNext()) {
			String cmd = scanner.next();
			anerisMsg = parseOneCmd(cmd, msgType);
			anerisMsg.setSlot(slot);
			anerisMsgs.add(anerisMsg);
		}

		return anerisMsgs;
	}

	/**
	 * Builds a list of Aneris messages from a string representation of the
	 * objects.
	 * 
	 * @throws IllegalArgumentException if the message is in the wrong format.
	 */
	public static LinkedList<AnerisMessage> parseString(String msg, long slot, AnerisType anerisType) {

		if (anerisType == AnerisType.INTERPRETED) {
			return parseStringInterpreted(msg, slot);

		} else if (anerisType == AnerisType.LISP) {
			return parseStringLisp(msg, slot);

		} else {
			throw new IllegalArgumentException("Unsupported aneris message type: " + msg);
		}
	}

	/**
	 * Returns the Nuprl string representation (understandable by Aneris) of
	 * the object.
	 */
	public String toNuprlString(AnerisType type) {
		if (type == AnerisType.INTERPRETED) {
			return String.format(ANERIS_SEND_MSG_FORMAT, msgType.toString(), uid, nuprlProposal());
		} else {
			return String.format(ANERIS_LISP_SEND_MSG_FORMAT, msgType.toString(), uid, nuprlProposal());
		}
	}

	public static String toNuprlString(AnerisType type, ANERIS_MSG_TYPE msgType, long commandId, String nuprlProposal) {
		if (type == AnerisType.INTERPRETED) {
			return String.format(ANERIS_SEND_MSG_FORMAT, msgType, commandId, nuprlProposal);
		} else {
			return String.format(ANERIS_LISP_SEND_MSG_FORMAT, msgType, commandId, nuprlProposal);
		}
	}

	/**
	 * Returns a byte-encoded form of this object that is understandable
	 * by the Aneris service and ready to be sent over the network.
	 */
	public byte[] getBytes(AnerisType type) {
		String nuprlString = toNuprlString(type);
		byte[] msgInBytes = nuprlString.getBytes();
		return (msgInBytes.length + "!" + nuprlString).getBytes();
	}

	/**
	 * Returns the Nuprl string representation of the value proposed only.
	 * The command id and message type are omitted.
	 */
	public String nuprlProposal() {
		StringBuffer sb = new StringBuffer();

		if (msgType == ANERIS_MSG_TYPE.BCAST) {
			sb.append("###" + msgType.name() + "#");
			sb.append("#" + proposerId + "#");
			sb.append("#" + seqNo + "#");
			for (IdIpPort member : members) {
				sb.append("#" + member.toNuPRLString() + "#");
			}
			sb.append("##");
		} else if (msgType == ANERIS_MSG_TYPE.DUMMY) {
			sb.append(ANERIS_MSG_TYPE.DUMMY.name());
			sb.append("###");
			sb.append(proposerId);
			sb.append("###");

		} else {
			sb.append(protocol.toString());
		}
		return sb.toString();
	}

	public String toString() {
		StringBuffer sb = new StringBuffer();

		sb.append("slot: " + slot + ", type: " + msgType.name());
	
		if (msgType == ANERIS_MSG_TYPE.BCAST) {
			sb.append("<proposerId: " + proposerId);
			sb.append(" seqNo: " + seqNo);
			sb.append(" members: (");
			for (IdIpPort member : members) {
				sb.append(" " + member);
			}
			sb.append(" )>");
		} else if (msgType == ANERIS_MSG_TYPE.SWAP) {
			sb.append(": " + protocol);
		} else if (msgType == ANERIS_MSG_TYPE.DUMMY) {
			sb.append(" proposer id: " + proposerId);
		}
		return sb.toString();
	}

	public ANERIS_MSG_TYPE getType() {
		return msgType;
	}

	public PROTOCOL_TYPE getProtocol() {
		return protocol;
	}

	public LinkedList<IdIpPort> getMembers() {
		return members;
	}

	public long getSeqNo() {
		return seqNo;
	}

	public String getProposerId() {
		return proposerId;
	}

	public long getSlot() {
		return slot;
	}

	private void setSlot(long slot) {
		this.slot = slot;
	}
}
