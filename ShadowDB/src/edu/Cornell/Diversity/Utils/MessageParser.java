package edu.Cornell.Diversity.Utils;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;

import edu.Cornell.Diversity.ShadowDB.ShadowDBConfig;

/**
 * A class used to parse and transfer messages over Java NIO channels.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class MessageParser {

	public static final int HEARTBEAT_HEADER = -1;
	public static final int ERROR_HEADER = -2;

	/**
	 * A value indicating that the header was not completely
	 * read yet.
	 */
	public static final int MSG_INCOMPLETE = -3;

	public static final int SUSPICION = -4;

	/**
	 * The channels this parser is associated with. Messages will be received
	 * an parsed from a channel and transferred to another one.
	 */
	protected ReadableByteChannel from;
	protected WritableByteChannel to;

	/**
	 * True when the parser is ready to parse the message's payload.
	 */
	protected boolean expectingPayload;
	protected int header;
	protected int payloadBytesTransferred;
	protected ByteBuffer buffer;

	/**
	 * True when the end point connected to on of the above channels has closed the channel
	 * or is suspected to have crashed.
	 */
	protected boolean endPointSuspected;

	public MessageParser(ReadableByteChannel from, WritableByteChannel to) {
		this.from = from;
		this.to = to;
		this.expectingPayload = false;
		this.endPointSuspected = false;
		this.header = MSG_INCOMPLETE;
		this.payloadBytesTransferred = 0;
		this.buffer = ByteBuffer.allocate(ShadowDBConfig.getMaxMsgSize());
		this.buffer.clear();
	}

	public MessageParser(ReadableByteChannel from) {
		this(from, null);
	}

	public boolean expectingPayload() {
		return expectingPayload;
	}

	public boolean endPointSuspected() {
		return endPointSuspected;
	}

	/**
	 * Returns true iff the passed header is a complete header.
	 */
	public static boolean headerValid(int msgHeader) {
		return (msgHeader != ERROR_HEADER) && (msgHeader != MSG_INCOMPLETE);
	}

	/**
	 * <p> Reads a header from the input channel.
	 * The header is returned as an integer. If the header could not be completely read
	 * {@code MSG_INCOMPLETE} is returned. If the input channel is closed, {@code ERROR_HEADER} is returned.
	 */
	public int parseHeader() throws IOException {
		if (expectingPayload) {
			throw new IllegalStateException("The message header has already been parsed!");
		} else {
			header = readHeader(from, buffer);

			if (header != ERROR_HEADER && header != MSG_INCOMPLETE && header != SUSPICION) {
				buffer.clear();

				if (header != HEARTBEAT_HEADER) {
					expectingPayload = true;
				} else {
					expectingPayload = false;
					payloadBytesTransferred = 0;
				}

			} else if (header == ERROR_HEADER || header == SUSPICION) {
				endPointSuspected = true;
			}
			return header;
		}
	}

	/**
	 * Transfers a message from a channel to another. This method should only be invoked
	 * after successfully reading the message's header. When the message has been
	 * transferred completely, true is returned (false is returned otherwise). 
	 */
	public boolean transferMessage() throws IOException {
		if (!expectingPayload) {
			throw new IllegalStateException("A message header must be parsed before attempting to transfer the message");
		} else {
			if (buffer.position() == 0) {
				buffer.limit(header + NIOUtils.BYTE_COUNT_INT);
				buffer.putInt(header);
			}

			int bytesTransferred = transferData(from, to, buffer);

			if (bytesTransferred == -1) {
				endPointSuspected = true;
				return false;
			}

			payloadBytesTransferred += bytesTransferred;

			if (payloadBytesTransferred == (header + NIOUtils.BYTE_COUNT_INT)) {
				expectingPayload = false;
				payloadBytesTransferred = 0;
				buffer.clear();
				return true;
			} else {
				return false;
			}
		}
	}

	public Object parseMessage() throws Exception {
		if (!expectingPayload) {
			throw new IllegalStateException("A message header must be parsed before attempting to parse the payload");
		} else {
			Object msg = null;

			if (buffer.position() == 0) {
				buffer.limit(header);
				System.out.println("Set limit to " + header);
			}

			int nbBytes = from.read(buffer);
			if (nbBytes == -1) {
				System.out.println("suspected endpoint because read -1 ");

				endPointSuspected = true;
				return msg;
			}

			System.out.println("read " + buffer.position() + " bytes");
			if (buffer.position() == header) {
				System.out.println("read " + header + " bytes, parsing message");

				expectingPayload = false;
				buffer.flip();
				msg = NIOUtils.deserializeObject(buffer);
				buffer.clear();
			}
			return msg;
		}
	}

	/**
	 * Transfer data from one channel to the other using the provided byte buffer. This method
	 * assumes that the proper limit has already been set on the provided buffer.
	 * 
	 * This method returns the number of bytes transferred.
	 */
	private static int transferData(ReadableByteChannel srcChannel, WritableByteChannel destChannel,
		ByteBuffer buffer) throws IOException {
	
		int bytesTransfered = srcChannel.read(buffer);		
	
		/**
		 * The other end has closed the connection. Return
		 * the error.
		 */
		if (bytesTransfered == -1) {
			return bytesTransfered;
		} else {
			bytesTransfered = 0;
		}
	
		buffer.flip();
	
		while (buffer.hasRemaining()) {
			bytesTransfered += destChannel.write(buffer);
		}
		
		return bytesTransfered;
	}

	/**
	 * Reads a header from the given channel using the provided buffer.
	 * The header is returned as a long. If the header could not be read,
	 * {@code ERROR_HEADER} is returned. If some but not all bytes were read
	 * {@code MSG_INCOMPLETE} is returned.
	 */
	private static int readHeader(ReadableByteChannel channel, ByteBuffer buff) throws IOException {
		buff.limit(NIOUtils.BYTE_COUNT_INT);
	
		int ret = channel.read(buff);
		if (buff.position() == NIOUtils.BYTE_COUNT_INT) {
			buff.flip();
			return buff.getInt();
		} else if (ret < 0) {
			return ERROR_HEADER;
		} else {
			return MSG_INCOMPLETE;
		}
	}
}
