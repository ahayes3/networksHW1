import java.io.{BufferedWriter, DataInputStream, DataOutputStream, File, FileWriter, IOException}
import java.net.{InetSocketAddress, Socket}
import java.nio.ByteBuffer
import java.nio.channels.{DatagramChannel, SocketChannel}
import java.nio.file.Files

import scala.io.StdIn
import scala.util.Random

object homework1 {
	val n = 50
	
	def main(args: Array[String]) {
		var host: String = ""
		var tcpEcho = 0
		var udpEcho = 0
		var msgPortTcp = 0
		var msgPortUdp = 0
		
		if (args.length == 5) {
			host = args(0)
			tcpEcho = args(1).toInt
			udpEcho = args(2).toInt
			msgPortTcp = args(3).toInt
			msgPortUdp = args(4).toInt
		}
		else {
			println("Enter the target address")
			host = StdIn.readLine
			println("Enter the tcp echo port")
			tcpEcho = scala.io.StdIn.readInt
			println("Enter the udp echo port")
			udpEcho = StdIn.readInt
			println("Enter the tcp msg size port")
			msgPortTcp = StdIn.readInt
			println("Enter the udp msg size port")
			msgPortUdp = StdIn.readInt
		}
		
		
		//		val result1 = test1(host, tcpEcho)
		//		val result2 = test2(host, udpEcho)
		//		val result3 = test3(host, tcpEcho)
		//		val result4 = test4(host, msgPortTcp)
		val result5 = test5(host, msgPortUdp)
		
		val fields = Array("Test1-8", "Tes1-64", "Test1-1024", "Test2-8", "Test2-64", "Test-1024", "Test3-1k", "Test3-16k",
			"Test3-64k", "Test3-256k", "Test3-1m", "Test4-1024", "Test4-2048", "Test4-4096", "Test5-1024", "Test5-2048", "Test5-4096")
		
		println("Enter path to save results.")
		val path = StdIn.readLine
		println("Enter name of file")
		val name = StdIn.readLine
		/*
		val outFile = new BufferedWriter(new FileWriter(path + File.separator + name + ".csv"))
		
		for (i <- fields.indices) {
			val list = i match {
				case 1 => result1(0)
				case 2 => result1(1)
				case 3 => result1(2)
				case 4 => result2(0)
				case 5 => result2(1)
				case 6 => result2(2)
				case 7 => result3(0)
				case 8 => result3(1)
				case 9 => result3(2)
				case 10 => result3(3)
				case 11 => result3(4)
				case 12 => result4(0)
				case 13 => result4(1)
				case 14 => result4(2)
				case 15 => result5(0)
				case 16 => result5(1)
				case 17 => result5(2)
			}
			
			outFile.write(fields(i))
			outFile.write(',')
			for (j <- list.indices) {
				outFile.write(j)
				if (j != list.length - 1)
					outFile.write(',')
				
			}
			outFile.newLine
		}
		outFile.close
		*/
	}
	
	def test1(host: String, port: Int): Array[Array[Long]] = {
		val echoChannel = SocketChannel.open
		echoChannel.connect(new InetSocketAddress(host, port))
		val rtts = Array.ofDim[Long](3, n)
		println("Test1: TCP latency")
		for (b <- rtts.indices) {
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
					val encoded = ByteBuffer.wrap(keyXor(value, key))
					//val encoded = value.mapInPlace(b => (b ^ key.toByte).toByte)
					val returned = ByteBuffer.wrap(new Array[Byte](byteNum))
					val sent = System.nanoTime
					echoChannel.write(encoded)
					echoChannel.read(returned)
					returned.flip
					val received = System.nanoTime
					println("Message received in " + (received - sent) + " nanoseconds")
					
					if (bytesMatch(value, keyXor(returned.array, key))) {
						validated = true
						println("Validated: " + validated)
						rtts(b)(a) = received - sent
					}
				}
			}
		}
		echoChannel.close
		
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
		val rtts = Array.ofDim[Long](3, n)
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
		val output = Array.ofDim[Double](5, n)
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
		val output = Array.ofDim[Long](3, n)
		
		//socketChannel.connect(new InetSocketAddress(host, port))
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
					
					println("Sending " + msg._1 + " messages of " + msg._2 + " length test " + (b + 1) + " of " + output(a).length)
					var sent = None: Option[Long]
					var received = None: Option[Long]
					var code = None: Option[Long]
					try {
						sent = Some(System.nanoTime)
						for (b <- buffers) {
							socketChannel.write(b)
						}
						val bytesRead = socketChannel.read(response)
						received = Some(System.nanoTime)
						println("read")
						if (bytesRead == -1)
							println("ERROR CHANNEL CLOSED")
						else if (bytesRead == 0)
							println("No bytes read")
						else if (bytesRead == -2)
							println("WTF")
						
						code = Some(response.getLong(0))
					}
					catch {
						case e: Exception => e.printStackTrace()
					}
					
					
					if (code.getOrElse(-1) == 741852963) {
						output(a)(b) = received.get - sent.get
						println("Validated")
						validated = true
					}
					else
						println("Not validated")
					
					socketChannel.close
				}
			}
		}
		output
	}
	
	def test5(host: String, port: Int): Array[Array[Long]] = {
		val output = Array.ofDim[Long](3, n)
		val mByte = 1048576
		
		
		for (a <- output.indices) {
			for (b <- output(a).indices) {
				var validated = false
				while (!validated) {
					val msg = a match {
						case 0 => (1024, 1024)
						case 1 => (2048, 512)
						case 2 => (4096, 256)
					}
					
					val udp = DatagramChannel.open
					//		udp.bind(new InetSocketAddress(host, port))
					udp.connect(new InetSocketAddress(host, port))
					udp.configureBlocking(false)
					
					val value = Random.nextBytes(mByte)
					val key = Random.nextBytes(8)
					val encoded = keyXor(value, key)
					val response = ByteBuffer.wrap(new Array[Byte](8))
					
					val buffers = for (i <- 0 until msg._1) yield {
						ByteBuffer.wrap(encoded slice(msg._2 * i, (msg._2 * i) + msg._2))
					}
					
					println("Sending " + msg._1 + " messages of " + msg._2 + " length test " + b + " of " + output(a).length)
					val sent = System.nanoTime
					for (b <- buffers) {
						udp.send(b, new InetSocketAddress(host, port))
						//Thread.sleep(50)
					}
					udp.receive(response)
					println("read")
					val received = System.nanoTime
					udp.close
					
					//response.flip
					val code = response.getLong(0)
					if (code == 123456789) {
						output(a)(b) = received - sent
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
