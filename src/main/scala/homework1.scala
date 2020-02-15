import java.io.{DataInputStream, DataOutputStream, IOException}
import java.net.{InetSocketAddress, Socket}
import java.nio.ByteBuffer
import java.nio.channels.{DatagramChannel, SocketChannel}

import scala.util.Random

object homework1 {
	def main(args: Array[String]) {
		
		println("Enter the target address")
		val host = scala.io.StdIn.readLine()
		println("Enter the port number for tests 1-3")
		val port1 = scala.io.StdIn.readInt()
		println("Enter the port number for test 4 & 5")
		val port2 = scala.io.StdIn.readInt()
		
		
		//val result1 = test1(host, port1)
		//val result2 = test2(host, port1)
		//val result3 = test3(host, port1)
		val result4 = test4(host, port2)
		
		
	}
	
	def test1(host: String, port: Int): Array[Array[Long]] = {
		val echoSocket = new Socket(host, port)
		val out = new DataOutputStream(echoSocket.getOutputStream)
		val in = new DataInputStream(echoSocket.getInputStream)
		val rtts = Array.ofDim[Long](3, 50)
		println("Test1: TCP latency")
		for (b <- rtts.indices) {
			//rtts(b) = new Array[(Long, Long)](50)
			for (a <- rtts(b).indices) {
				var validated = false
				while (!validated) {
					//	val key = Random.nextLong
					val byteNum: Int = b match {
						case 0 => 8
						case 1 => 64
						case 2 => 1024
					}
					val value = Random.nextBytes(byteNum)
					val key = Random.nextBytes(8)
					val encoded = keyXor(value, key)
					//val encoded = value.mapInPlace(b => (b ^ key.toByte).toByte)
					val returned: Array[Byte] = new Array[Byte](byteNum)
					val sent = System.nanoTime
					out.write(encoded)
					
					in.read(returned)
					val received = System.nanoTime
					println("Message received in " + (received - sent) + " nanoseconds")
					
					if (bytesMatch(value, returned)) {
						validated = true
						println("Validated: " + validated)
						rtts(b)(a) = received - sent
					}
				}
			}
		}
		echoSocket.close
		in.close
		out.close
		
		for (a <- rtts.indices) {
			for (b <- rtts(a).indices) {
				a match {
					case 0 => println("Message size: 8 bytes         Time: " + rtts(a)(b) + " nanoseconds")
					case 1 => println("Message size: 64 bytes        Time: " + rtts(a)(b) + " nanoseconds")
					case 2 => println("Message size: 1024 bytes      Time: " + rtts(a)(b) + " nanoseconds")
				}
			}
		}
		rtts
	}
	
	def test2(host: String, port: Int): Array[Array[Long]] = {
		val udp = DatagramChannel.open
		udp.connect(new InetSocketAddress(host, port))
		val rtts = Array.ofDim[Long](3, 50)
		println("Test2: UDP latency")
		
		for (a <- rtts.indices) {
			for (b <- rtts(a).indices) {
				var validated = false
				while (!validated) {
					//val key = Random.nextLong
					val byteNum = a match {
						case 0 => 8
						case 1 => 64
						case 2 => 1024
					}
					
					val value = Random.nextBytes(byteNum)
					val key = Random.nextBytes(8)
					val encoded = keyXor(value, key)
					//val encoded = value.map(b => (b ^ key.toByte).toByte)
					val returned = ByteBuffer.wrap(new Array[Byte](byteNum))
					val sent = System.nanoTime
					udp.write(ByteBuffer.wrap(encoded))
					udp.read(returned)
					val received = System.nanoTime()
					
					val decoded = keyXor(returned.array, key)
					println("received in " + (received - sent) + " nanoseconds")
					
					if (bytesMatch(decoded, value)) {
						validated = true
						println("Validated")
						rtts(a)(b) = received - sent
					}
					else
						println("Not validated")
					
				}
			}
		}
		rtts
		
	}
	
	def test3(host: String, port: Int): Array[Array[Double]] = { //Value in bytes per second
		val output = Array.ofDim[Double](5, 50)
		val socketChannel = SocketChannel.open(new InetSocketAddress(host, port))
		
		
		println("Test 3: Throughput")
		
		for (a <- output.indices) {
			for (b <- output.indices) {
				val byteNum = a match {
					case 0 => 1024
					case 1 => 16384
					case 2 => 65536
					case 3 => 262144
					case 4 => 1048576
				}
				var validated = false
				while (!validated) {
					println("Sending " + byteNum + " bytes")
					val value = Random.nextBytes(byteNum)
					val key = Random.nextBytes(8)
					val encoded = keyXor(value, key)
					//val encoded = ByteBuffer.wrap(value.map(b => (b ^ key.toByte).toByte))
					val returned = ByteBuffer.wrap(new Array[Byte](byteNum))
					
					val sent = System.nanoTime
					socketChannel.write(ByteBuffer.wrap(encoded))
					while (socketChannel.read(returned) > 0) {}
					
					//println("Read " + socketChannel.read(returned) + " bytes")
					val received = System.nanoTime
					
					//val returned2 = new Array[Byte](byteNum)
					val decoded = keyXor(returned.array, key)
					//val decoded = returned2.map(b => (b ^ key.toByte).toByte)
					
					if (bytesMatch(decoded, value)) {
						validated = true
						output(a)(b) = (1000000.toLong * byteNum.toLong) / ((received - sent) / 2).toDouble //.asInstanceOf[Double]
						println("Sent and received " + byteNum + " bytes in " + (received - sent) + " nanoseconds. Calculated " + output(a)(b) + " kbytes per second")
					}
					else
						println("Validation failed")
				}
			}
		}
		output
	}
	
	def test4(host: String, port: Int): Array[Array[Long]] = {
		var output = Array.ofDim[Long](3, 50)
		
		for (a <- output.indices) {
			for (b <- output(a).indices) {
				var validated = false
				while (!validated) {
					val msg = a match {
						case 0 => (1024, 1024)
						case 1 => (2048, 512)
						case 2 => (4096, 256)
					}
					val socketChannel = SocketChannel.open(new InetSocketAddress(host, port))
					
					val value: scala.Array[Byte] = Random.nextBytes(1048576)
					val key: scala.Array[Byte] = Random.nextBytes(8)
					val encoded: scala.Array[Byte] = keyXor(value, key)
					val response = ByteBuffer.wrap(new Array[Byte](8))
					
					
					val buffers = for (i <- 0 until msg._1) yield {
						ByteBuffer.wrap(encoded slice(msg._2 * i, (msg._2 * i) + msg._2))
					}
					
					println("Sending " + msg._1 + " messages of " + msg._2 + " length")
					var sent = None: Option[Long]
					var received = None: Option[Long]
					var code = None: Option[Long]
					try {
						sent = Some(System.nanoTime)
						for (b <- buffers) {
							socketChannel.write(b)
						}
						
						socketChannel.read(response)
						socketChannel.close
						received = Some(System.nanoTime)
						code = Some(response.getLong(0))
					}
					catch {
						case e: IOException => println("IOException")
					}
					
					
					if (code.getOrElse(-1) == 123456789) {
						output(a)(b) = received.get - sent.get
						println("Validated")
						validated = true
					}
					else
						println("Not validated")
				}
			}
		}
		output
	}
	
	def test5(host: String,port:Int):Array[Array[Long]] = {
		val output = new Array[Array[Long]](3)
		
		output
	}
	
	def bytesMatch(a: Array[Byte], b: Array[Byte]): Boolean = {
		if (a.length != b.length)
			return false
		
		var matches = true
		for (i <- a.indices) {
			if (a(i) != b(i)) {
				matches = false
				println("Mismatch at index " + i)
				println(a(i) + " " + b(i))
			}
		}
		matches
	}
	
	def keyXor(value: Array[Byte], key: Array[Byte]): Array[Byte] = {
		val output = new Array[Byte](value.length)
		for (i <- value.indices by 8) {
			for (j <- key.indices) {
				output(i + j) = (value(i + j) ^ key(j)).asInstanceOf[Byte]
			}
		}
		output
	}
	
	def bytes2long(bytes: Array[Byte]): Long = {
		var output: Long = 0
		for (i <- 0 until 8) {
			output <<= 8
			output |= (bytes(i) & 0xff)
		}
		output
	}
}
