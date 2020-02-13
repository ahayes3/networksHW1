import java.io.{DataInputStream, DataOutputStream}
import java.net.{DatagramSocket, InetSocketAddress, Socket}
import java.nio.ByteBuffer
import java.nio.channels.{DatagramChannel, SocketChannel}

import scala.collection.mutable.ListBuffer
import scala.io.StdIn
import scala.util.Random

object EchoTest {
	def main(args: Array[String]) {
		val echoPort = 7
		val host = "gee.cs.oswego.edu"
		
		val result1 = test1(host, echoPort)
		val result2 = test2(host,echoPort)
		
		
		
		//End tcp 1
		
		
	}
	
	def test1(host: String, echoPort: Int): Array[Array[Long]] = {
		val echoSocket = new Socket(host, echoPort)
		val out = new DataOutputStream(echoSocket.getOutputStream)
		val in = new DataInputStream(echoSocket.getInputStream)
		val rtts = Array.ofDim[Long](3, 50)
		println("Test1: TCP latency")
		for (b <- 0 to 2) {
			//rtts(b) = new Array[(Long, Long)](50)
			for (a <- 0 to 49) {
				var validated = false
				while (!validated) {
					val key = Random.nextLong
					val byteNum: Int = b match {
						case 0 => 8
						case 1 => 64
						case 2 => 1024
					}
					val value = Random.nextBytes(byteNum)
					val encoded = value.mapInPlace(b => (b ^ key.toByte).toByte)
					val returned: Array[Byte] = new Array[Byte](byteNum)
					val sent = System.nanoTime
					out.write(encoded)
					
					in.read(returned)
					val recieved = System.nanoTime
					println("Message recieved in " + (recieved - sent) + " nanoseconds")
					
					if (bytesMatch(value, returned)) {
						validated = true
						println("Validated: " + validated)
						rtts(b)(a) = recieved - sent
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
	
	def test2(host: String, echoPort: Int): Array[Array[Long]] = {
		val udp = DatagramChannel.open
		udp.connect(new InetSocketAddress(host, echoPort))
		val rtts = Array.ofDim[Long](3, 50)
		println("Test2: UDP latency")
		
		for (a <- 0 to 2) {
			for (b <- 0 to 49) {
				var validated = false
				while (!validated) {
					val key = Random.nextLong
					val byteNum = a match {
						case 0 => 8
						case 1 => 64
						case 2 => 1024
					}
					
					val value = Random.nextBytes(byteNum)
					val encoded = value.mapInPlace(b => (b ^ key.toByte).toByte)
					val returned = ByteBuffer.allocate(byteNum)
					val sent = System.nanoTime
					udp.write(ByteBuffer.wrap(encoded))
					udp.read(returned)
					val recieved = System.nanoTime()
					val decoded = returned.array.mapInPlace(b => (b ^ key.toByte).toByte)
					println("Recieved in " + (recieved - sent) + " nanoseconds")
					
					if (bytesMatch(decoded, value)) {
						validated = true
						println("Validated")
						rtts(b)(a) = recieved - sent
					}
					else
						println("Not validated")
					
				}
			}
		}
		rtts
		
	}
	
	def test3()
	
	
	
	def bytesMatch(a: Array[Byte], b: Array[Byte]): Boolean = {
		if (a.length != b.length)
			return false
		
		var matches = true
		for (i <- a.indices) {
			if (a(i) != b(i))
				matches = false
		}
		matches
	}
}
