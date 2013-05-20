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
 *  o File name:   NIOUtils.java
 *  o Description: A utility class to interact with non-blocking channels.
 */

package edu.Cornell.Diversity.Utils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.nio.ByteBuffer;
import java.nio.channels.Pipe;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * A utility class to work with asynchronous channels.
 * 
 * The format of object in serialized form is a header followed
 * by the object. The header is the number of bytes the object
 * takes in serialized form.
 * 
 * @author nschiper@cs.cornell.edu
 */
public class NIOUtils {

	/**
	 *  The number of bytes of a long.
	 */
	public static final int BYTE_COUNT_LONG = Long.SIZE / 8;

	/**
	 * The number of bytes of an int.
	 */
	public static final int BYTE_COUNT_INT = Integer.SIZE / 8;

	/**
	 * An error indicating that a header could not be read.
	 */
	public static final long ERROR_HEADER_LONG = -2;

	public static final int ERROR_HEADER_INT = -2;

	/**
	 * When reading from an Aneris channel, we will retry a couple
	 * of times before giving up and considering the other end crashed.
	 * More specifically, we will retry {@code RETRY_COUNT} times and
	 * sleep {@code SLEEP_TIME_BETWEEN_TRIALS} number of times.
	 */
	private static final int RETRY_COUNT = 10;
	private static final int SLEEP_TIME_BETWEEN_TRIALS = 1000;

	/**
	 * A byte marking the end of a header. This is used when communication
	 * with Aneris.
	 */
	public static byte END_BYTE = (byte) (((int) '!') >> 24);

	/**
	 * Writes the content of the buffer to the given channel.
	 */
	public static void writeToChannel(WritableByteChannel chan, ByteBuffer buffer) throws IOException {
		buffer.rewind();

		while (buffer.hasRemaining()) {
			chan.write(buffer);
		}
	}

	/**
	 * Writes the object to the channel using the provided byte buffer for serialization.
	 * Returns a boolean indicating whether the operation was successful.
	 */
	public static boolean writeToChannel(WritableByteChannel chan, Object obj, ByteBuffer buffer)
		throws IOException {

		boolean success = serializeObject(obj, buffer, false /* markEndOfHeader */);

		if (success) {
			while (buffer.hasRemaining()) {
				chan.write(buffer);
			}
		}
		return success;
	}

	/**
	 * Transfer noBytes bytes from one channel to the other using the provided byte buffer.
	 * The byte count does not include the header that denotes the number of bytes the object
	 * takes in serialized form.
	 * 
	 * This method returns a boolean indicating whether the transfer was successful.
	 */
	public static boolean transferData(long nbBytes, ReadableByteChannel from, WritableByteChannel to,
		ByteBuffer buffer) throws IOException {

		buffer.clear();

		// We first put the number of bytes of the object in serialized form.
		buffer.putLong(nbBytes);

		long bytesTransfered = 0;

		while (bytesTransfered <  (nbBytes + BYTE_COUNT_LONG)) {
			if (bytesTransfered != 0) {
				buffer.clear();
			}

			// Make sure we do not read too much from the channel
			long noBytesRemaining = (nbBytes + BYTE_COUNT_LONG) - bytesTransfered;

			if (noBytesRemaining < buffer.capacity()) {
				// Buffers are never bigger than 2GB, it is
				// thus safe to cast to int.
				buffer.limit((int) noBytesRemaining);
			}

			while (buffer.hasRemaining()) {
				if (from.read(buffer) < 0) {
					return false;
				}
			}

			buffer.flip();

			boolean notifiedThread = false;

			while (buffer.hasRemaining()) {
				bytesTransfered += to.write(buffer);

				/**
				 *  Notify the thread that is waiting to read from this pipe.
				 *  It is necessary to do this when the writable channel is full
				 *  and cannot be written unless a thread consumes part of what
				 *  was written.
				 */
				if (to instanceof Pipe.SinkChannel && !notifiedThread) {
					notifiedThread = true;

					synchronized (to) {
						to.notify();
					}
				}
			}
		}
		return true;
	}

	/**
	 * Reads a header from the given channel using the provided buffer.
	 * The header is returned as a long. If the header could not be read,
	 * {@code ERROR_HEADER} is returned.
	 */
	public static long readHeader(ReadableByteChannel from, ByteBuffer buffer) throws IOException {
		buffer.clear();
		buffer.limit(BYTE_COUNT_LONG);

		while(buffer.hasRemaining()) {
			if (from.read(buffer) < 0) {
				return ERROR_HEADER_LONG;
			}
		}
		buffer.flip();

		return buffer.getLong();
	}

	/**
	 * <p> Reads a header from the given channel connected to the Aneris service using the provided buffer.
	 * The header is returned as an integer. If the header could not be read,
	 * {@code ERROR_HEADER_INT} is returned.
	 * 
	 * <p> This method uses an "ad-hoc" way to detect that the end host crashed: if after reading data
	 * {@value #RETRY_COUNT} times from the channel (and sleeping {@value #SLEEP_TIME_BETWEEN_TRIALS} ms between
	 * trials) no bytes could be read, we consider the end host dead and return {@code ERROR_HEADER_INT}.
	 */
	public static int readAnerisHeader(ReadableByteChannel from, ByteBuffer buffer) throws Exception {

		int headerLength = 0;
		int trialNo = 1;

		do {
			buffer.position(headerLength);
			headerLength++;
			buffer.limit(headerLength);

			while (buffer.hasRemaining()) {
				int bytesRead = from.read(buffer);
				if (bytesRead == 0) {
					Thread.sleep(SLEEP_TIME_BETWEEN_TRIALS);
					trialNo++;
					if (trialNo == RETRY_COUNT) {
						return ERROR_HEADER_INT;
					}
				}
				else if (bytesRead < 0) {
					return ERROR_HEADER_INT;
				}
			}
		} while (buffer.get(headerLength - 1) != '!');

		// The returned header does not include the '!'.
		byte[] headerBytes = new byte[headerLength - 1];
		buffer.flip();
		buffer.get(headerBytes);
		String headerStr = new String(headerBytes);
		return Integer.parseInt(headerStr);
	}

	/**
	 * <p> Reads the next Aneris message from this channel and returns it in String form.
	 * 
	 * <p> This method uses an "ad-hoc" way to detect that the end host crashed: if after reading data
	 * {@value #RETRY_COUNT} times from the channel (and sleeping {@value #SLEEP_TIME_BETWEEN_TRIALS} ms between
	 * trials) no bytes could be read, we consider the end host dead and return null.
	 * 
	 * <p> This method also returns null if the other end closes the connection before we read
	 * the string in its entirety.
	 * 
	 * @throws Exception if a problem occurred while reading the message.
	 */
	public static String readFromAnerisChannel(ReadableByteChannel chan, ByteBuffer buffer) throws Exception {

		int nbBytes = readAnerisHeader(chan, buffer);

		if (nbBytes != ERROR_HEADER_INT) {
			buffer.position(0);
			buffer.limit(nbBytes);

			int trialNo = 1;

			while (buffer.hasRemaining()) {
				int bytesRead = chan.read(buffer);
				if (bytesRead == 0) {
					if (trialNo < RETRY_COUNT) {
						trialNo++;
						Thread.sleep(SLEEP_TIME_BETWEEN_TRIALS);
					} else {
						return null;
					}
				}
				else if (chan.read(buffer) < 0) {
					return null;
				}
			}
			buffer.flip();
			byte[] msgInBytes = new byte[nbBytes];
			buffer.get(msgInBytes);
			String msg = new String(msgInBytes);

			return msg;
		} else {
			return null;
		}
	}

	/**
	 * Returns the next object to be read from this channel, or null if
	 * no data can be read at this moment. This method assumes that the
	 * channel is in non-blocking mode and uses the provided buffer to
	 * read the object in serialized form from the channel. It is assumed
	 * that the buffer is big enough to hold the entire object in serialized form.
	 */
	public static Object readObjFromChannel(ReadableByteChannel chan, ByteBuffer buffer,
		AtomicBoolean endPointCrashed) throws Exception {

		buffer.clear();
		buffer.limit(BYTE_COUNT_LONG);

		if (chan.read(buffer) > 0) {
			while (buffer.hasRemaining()) {
				chan.read(buffer);
			}
			buffer.flip();
			long nbBytes = buffer.getLong();

			buffer.clear();
			// We never transfer objects of more than 2GB,
			// it is thus safe to type cast to int.
			buffer.limit((int) nbBytes);
			while (buffer.hasRemaining()) {
				if (chan.read(buffer) <= 0 && endPointCrashed.get()) {
					return null;
				}

			}
			buffer.flip();
			return deserializeObject(buffer);
		} else {
			return null;
		}
	}

	/**
	 * Returns the next object to be read from this channel, or null if
	 * no data can be read at this moment. This method assumes that the
	 * channel is in non-blocking mode and uses the provided buffer to
	 * read the object in serialized form from the channel. It is assumed
	 * that the buffer is big enough to hold the entire object in serialized form and of
	 * length byteCount.
	 */
	public static Object readObjFromChannel(ReadableByteChannel chan, int byteCount, ByteBuffer buffer,
		AtomicBoolean endPointCrashed) throws Exception {

		buffer.clear();
		buffer.limit(byteCount);

		if (chan.read(buffer) > 0) {
			while (buffer.hasRemaining()) {
				if (chan.read(buffer) <= 0 && endPointCrashed.get()) {
					return null;
				}
			}
			buffer.flip();
			return deserializeObject(buffer);
		} else {
			return null;
		}
	}

	/**
	 * This method serializes the given object and stores it in the provided
	 * byte buffer. It is assumed that the buffer is big enough to hold
	 * the entire object.
	 * 
	 * When the boolean markEndOfHeader is true, the header of the message (the size
	 * of the message) is followed by a '!'. This is used to send messages to Aneris.
	 */
	public static boolean serializeObject(Object obj, ByteBuffer buffer,
		boolean markEndOfHeader) throws IOException {

		ByteArrayOutputStream byteOutputStream = new ByteArrayOutputStream();
		ObjectOutputStream objOutputStream = new ObjectOutputStream(byteOutputStream);

		objOutputStream.writeObject(obj);
		byte[] objAsByteArray = byteOutputStream.toByteArray();
		objOutputStream.close();

		if (markEndOfHeader) {
			if ((objAsByteArray.length + BYTE_COUNT_LONG + Byte.SIZE) > buffer.capacity()) {
				return false;
			}
		} else {
			if ((objAsByteArray.length + BYTE_COUNT_LONG) > buffer.capacity()) {
				return false;
			}
		}

		// First write the number of bytes the object takes in serialized form.
		buffer.clear();
		buffer.putLong(objAsByteArray.length);
		if (markEndOfHeader) {
			buffer.put(END_BYTE);
		}
		buffer.put(objAsByteArray);

		buffer.flip();
		return true;
	}

	public static byte[] serializeObject(Object obj) throws IOException {
		ByteArrayOutputStream outByteArray = new ByteArrayOutputStream();
		ObjectOutputStream out = new ObjectOutputStream(outByteArray);
		out.writeObject(obj);
		return outByteArray.toByteArray();
	}

	/** 
	 * This method deserializes an object stored in byte buffer form.
	 */
	public static Object deserializeObject(ByteBuffer obj) throws Exception {
		byte[] objAsByteArray = new byte[obj.limit()];
		obj.get(objAsByteArray);
		
		ByteArrayInputStream byteInputStream = new ByteArrayInputStream(objAsByteArray);
		ObjectInputStream objInputStream = new ObjectInputStream(byteInputStream);
		Object result = objInputStream.readObject();
		objInputStream.close();

		return result;		
	}
}
