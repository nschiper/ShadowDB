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

import java.sql.Connection;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Scanner;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import edu.Cornell.Diversity.ShadowDB.QueryResult;
import edu.Cornell.Diversity.ShadowDB.ShadowDBConfig;
import edu.Cornell.Diversity.ShadowDB.TransactionId;
import edu.Cornell.Diversity.TOBroadcast.TobcastClient.AnerisType;
import edu.Cornell.Diversity.Test.MiniTransaction;
import edu.Cornell.Diversity.Utils.DbUtils;
import edu.Cornell.Diversity.Utils.IdIpPort;

/**
 * This class represents messages sent/received to/from the Aneris Diversified
 * ordered broadcast service.
 * 
 * This message is either a group reconfiguration request, a protocol change, or
 * a mini-transaction.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class AnerisMessage {

	private static Pattern CMD_SEPARATOR = Pattern.compile("\\#\\#\\#(.)+\\#\\#\\#");

	private static Pattern ID_PATTERN = Pattern.compile("\\p{Alpha}+\\d+");
	private static Pattern INTEGER_PATTERN = Pattern.compile("\\#(\\d+)\\#");
	private static Pattern PORT_PATTERN = Pattern.compile("\\-(\\d+)\\#");
	private static Pattern IP_PATTERN = Pattern.compile("\\-(\\d+\\-\\d+\\-\\d+\\-\\d+)\\-");
	private static Pattern NATURAL_NUMBER_PATTERN = Pattern.compile("natural_number:,\\d+:n");
	private static String NATURAL_NUMBER_STRING = "natural_number:,";
	private static String LISP_DELIMETER = "FST \\d+";

	// This is used to generate unique ids for broadcast messages.
	private static AtomicLong id = new AtomicLong(0);

	// The proposerId of a swap message is a "FakeId0".
	// The ids of such messages will thus start from 0.
	private static String FAKE_ID = "FakeId0";

	/**
	 * The Nuprl term format to send messages to Aneris.
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

	public enum MessageType {
		// For Aneris, transactions are also broadcast messages.
		BCAST("bcast"), SWAP("swap"), TRANS("bcast"), DUMMY("bcast");

		private String name;

		private MessageType(String name) {
			this.name = name;
		}

		public String toString() {
			return name;
		}
	};

	// Variables for swap messages
	public enum ProtocolType {
		PAXOS("paxos"), TWOTHIRD("2/3");

		private String name;

		private ProtocolType(String name) {
			this.name = name;
		}

		public String toString() {
			return name;
		}
	}

	private MessageType msgType;

	private ProtocolType protocol;

	private long uid;

	// Variables for bcast messages
	private LinkedList<IdIpPort> members;
	private long seqNo;
	private String proposerId;

	// Variables for mini-transactions
	private String clientId;
	private LinkedList<Integer> keysToRead;
	private HashMap<Integer, Integer> keysToWrite;

	// The index (or slot) of the Aneris message
	private long slot;

	public AnerisMessage(ProtocolType protocol) {
		this.msgType = MessageType.SWAP;
		this.protocol = protocol;
		this.proposerId = FAKE_ID;
		this.uid = DbUtils.extractIntFromId(proposerId) + (ShadowDBConfig.getMaxClientCount() * id.incrementAndGet());
	}

	public AnerisMessage(LinkedList<IdIpPort> members, long seqNo, String proposerId) {
		this.msgType = MessageType.BCAST;
		this.members = members;
		this.seqNo = seqNo;
		this.proposerId = proposerId;
		this.uid = DbUtils.extractIntFromId(proposerId) + (ShadowDBConfig.getMaxClientCount() * id.incrementAndGet());
	}

	public AnerisMessage(String clientId, LinkedList<Integer> keysToRead,
		HashMap<Integer, Integer> keysToWrite) {

		this.msgType = MessageType.TRANS;
		this.clientId = clientId;
		this.keysToRead = keysToRead;
		this.keysToWrite = keysToWrite;
		this.proposerId = FAKE_ID;
		this.uid = DbUtils.extractIntFromId(clientId) + (ShadowDBConfig.getMaxClientCount() * id.incrementAndGet());
	}

	/**
	 * Builds an empty Aneris message.
	 */
	public AnerisMessage(String proposerId) {
		this.msgType = MessageType.DUMMY;
		this.proposerId = proposerId;
		this.uid = DbUtils.extractIntFromId(proposerId) + (ShadowDBConfig.getMaxClientCount() * id.incrementAndGet());
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

	public static AnerisMessage parseMiniTransaction(String miniTransaction) {

		String[] tokens = miniTransaction.split("[$]");

		// The 2nd token is the clientId
		String clientId = tokens[1].trim();

		// The next token is a sequence of keys to read separated by '&'.
		LinkedList<Integer> keysToRead = new LinkedList<Integer>();

		if (!tokens[2].isEmpty()) {
			String[] toRead = tokens[2].split("[&]"); 
	
			for (String token : toRead) {
				if (!token.isEmpty()) {
					keysToRead.add(Integer.parseInt(token));
				}
			}
		}

		// The next token is a sequence of key value pairs to write separated by '&'.
		HashMap<Integer, Integer> keysToWrite = new HashMap<Integer, Integer>();

		if (!tokens[3].isEmpty()) {
			String[] toWrite = tokens[3].split("[&]");
	
			for (int i = 0; i < toWrite.length; i+= 2) {
				int key = Integer.parseInt(toWrite[i]);
				int value = Integer.parseInt(toWrite[i + 1]);
				keysToWrite.put(key, value);
			}
		}
		return new AnerisMessage(clientId, keysToRead, keysToWrite);
	}

	/**
	 * Parses an aneris message when it contains a protocol.
	 */
	private static AnerisMessage parseProtocol(String msg) {
		ProtocolType protocol = null;
		
		if (msg.contains(ProtocolType.PAXOS.toString())) {
			protocol = ProtocolType.PAXOS;
		} else if (msg.contains(ProtocolType.TWOTHIRD.toString())) {
			protocol = ProtocolType.TWOTHIRD;
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
	private static AnerisMessage parseOneCmd(String msg, MessageType msgType) {
		AnerisMessage anerisMsg;

		if (msgType == MessageType.BCAST || msgType == MessageType.TRANS) {

			Matcher matcher = CMD_SEPARATOR.matcher(msg);

			if (matcher.find()) {
				String msgContent = matcher.group();

				if (msg.contains(MessageType.DUMMY.name())) {
					anerisMsg = parseDummy(msgContent);
				} else if (msgContent.contains(MessageType.BCAST.name())) {
					anerisMsg = parseConfiguration(msgContent);
				} else if (msgContent.contains(MessageType.TRANS.name())) {
					anerisMsg = parseMiniTransaction(msgContent);
				} else {
					throw new IllegalArgumentException("Unknown aneris message type: " + msg);
				}
			}  else {
				throw new IllegalArgumentException("Incorrect syntax for decided command: " + msg);
			}
		
		} else if (msgType == MessageType.SWAP) {
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
				if (scanner.hasNextInt()) {
					index = scanner.nextInt();
					return index;
				} else {
					String token = scanner.next();
				}
			}
		}
		throw new IllegalArgumentException("Unsupported aneris type: " + anerisType);
	}

	/**
	 * Parses a message when received from the interpreted version
	 * of Aneris. This may contain multiple batched commands.
	 */
	private static LinkedList<AnerisMessage> parseStringInterpreted(String msg, long slot) {
		LinkedList<AnerisMessage> anerisMsgs = new LinkedList<AnerisMessage>();
		AnerisMessage anerisMsg;
	
		MessageType msgType;
		if (msg.contains(MessageType.DUMMY.name()) ||
			msg.contains(MessageType.BCAST.name()) ||
			msg.contains(MessageType.TRANS.name())) {

			msgType = MessageType.BCAST;

			String[] tokens = msg.split(NATURAL_NUMBER_STRING);

			/**
			 * The first natural number if the slot number, then
			 * comes the list of decided commands.
			 */
			for (int i = 2; i < tokens.length; i++) {
				anerisMsg = parseOneCmd(tokens[i], msgType);
				anerisMsg.setSlot(slot);
				anerisMsgs.add(anerisMsg);
			}

			return anerisMsgs;

		} else if (msg.contains("inl")) {
			// There's no batching for protocol change messages.
			anerisMsgs.add(parseProtocol(msg));
			return anerisMsgs;

		} else {
			throw new IllegalArgumentException("Received aneris message has wrong format: " + msg);
		}
	}

	/**
	 * Parses a message when received from the Lisp version of Aneris. This message
	 * may contain multiple commands.
	 */
	private static LinkedList<AnerisMessage> parseStringLisp(String msg, long slot) {
		LinkedList<AnerisMessage> anerisMsgs = new LinkedList<AnerisMessage>();
		AnerisMessage anerisMsg;
		MessageType msgType;

		if (msg.contains(MessageType.DUMMY.name()) ||
			msg.contains(MessageType.BCAST.name()) ||
			msg.contains(MessageType.TRANS.name())) {

			msgType = MessageType.BCAST;

			String[] tokens = msg.split(LISP_DELIMETER);

			/**
			 * The first natural number if the slot number, then
			 * comes the list of decided commands.
			 */
			for (int i = 2; i < tokens.length; i++) {
				anerisMsg = parseOneCmd(tokens[i], msgType);
				anerisMsg.setSlot(slot);
				anerisMsgs.add(anerisMsg);
			}

		} else {
			anerisMsgs.add(parseOneCmd(msg, MessageType.SWAP));
			return anerisMsgs;
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

	public static String toNuprlString(AnerisType type, MessageType msgType, long commandId, String nuprlProposal) {
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

		if (msgType == MessageType.BCAST) {
			sb.append("###" + msgType.name() + "#");
			sb.append("#" + proposerId + "#");
			sb.append("#" + seqNo + "#");
			for (IdIpPort member : members) {
				sb.append("#" + member.toNuPRLString() + "#");
			}
			sb.append("##");
		} else if (msgType == MessageType.TRANS) {
			sb.append("###" + msgType.name() + "$");
			sb.append(clientId + "$");

			int keyCount = 0;
			for (int key : keysToRead) {
				keyCount++;
				if (keyCount < keysToRead.size()) {
					sb.append(key + "&");
				} else {
					sb.append(key);
				}
			}
			sb.append("$");

			keyCount = 0;
			for (int key : keysToWrite.keySet()) {
				keyCount++;
				if (keyCount < keysToWrite.size()) {
					sb.append(key + "&" + keysToWrite.get(key) + "&");
				} else {
					sb.append(key + "&" + keysToWrite.get(key));
				}
			}
			sb.append("$###");

		} else if (msgType == MessageType.DUMMY) {
			sb.append(MessageType.DUMMY.name());
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
	
		if (msgType == MessageType.BCAST) {
			sb.append("<proposerId: " + proposerId);
			sb.append(" seqNo: " + seqNo);
			sb.append(" members: (");
			for (IdIpPort member : members) {
				sb.append(" " + member);
			}
			sb.append(" )>");
		} else if (msgType == MessageType.TRANS) {
			sb.append(" clientId: " + clientId);
			sb.append(" <keys to read: { ");

			for (int key : keysToRead) {
				sb.append(key + " ");
			}
			sb.append("}, keys to write: { ");
			for (int key : keysToWrite.keySet()) {
				sb.append("(" + key + ", " + keysToWrite.get(key) + ") ");
			}
			sb.append("} >");

		} else if (msgType == MessageType.SWAP) {
			sb.append(": " + protocol);
		} else if (msgType == MessageType.DUMMY) {
			sb.append(" proposer id: " + proposerId);
		}
		return sb.toString();
	}

	public MessageType getType() {
		return msgType;
	}

	public ProtocolType getProtocol() {
		return protocol;
	}

	public LinkedList<IdIpPort> getMembers() {
		return members;
	}

	public long getSeqNo() {
		return seqNo;
	}

	public long getCmdId() {
		return uid;
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

	public LinkedList<Integer> getKeysToRead() {
		return keysToRead;
	}

	public HashMap<Integer, Integer> getKeysToWrite() {
		return keysToWrite;
	}

	public String getClientId() {
		return clientId;
	}

	/**
	 * Executes the stored procedure reflected by this transaction.
	 */
	public QueryResult execute(Connection conn, TransactionId id) throws SQLException {
		if (msgType == MessageType.TRANS) {
			MiniTransaction trans = new MiniTransaction(id, keysToRead, keysToWrite);
			return trans.executeSql(conn);
			
		} else {
			throw new UnsupportedOperationException("Cannot execute aneris message of type: " + msgType);
		}
	}
}
