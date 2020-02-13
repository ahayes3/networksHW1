import java.io.{DataInputStream, DataOutputStream}
import java.net.Socket
import java.nio.ByteBuffer

import scala.collection.mutable.ListBuffer
import scala.io.StdIn
import scala.util.Random

object EchoTest
{
	def main(args: Array[String]) //LIST BUFFERS
	{
		val echoPort = 7
		val host = "gee.cs.oswego.edu"

		//var times = new Array[(Long, Long)](50)
		val sizes = new Array[Array[(Long, Long)]](3)

		println("Enter buffer size.")
		val bufferSize = StdIn.readInt()
		val buffer = ByteBuffer.allocateDirect(bufferSize)

		val echoSocket = new Socket(host, echoPort)
		val out = new DataOutputStream(echoSocket.getOutputStream)
		val in = new DataInputStream(echoSocket.getInputStream)

		for (b <- 0 to 2)
		{
			sizes(b) = new Array[(Long, Long)](50)
			for (a <- 0 to 49)
			{
				val key = Random.nextLong()
				val byteNum: Int = b match
				{
					case 0 => 8
					case 1 => 64
					case 2 => 1024
				}
				val value: Array[Byte] = Random.nextBytes(byteNum)
				var encoded:Array[Byte] = value.map(b => b ^ key).asInstanceOf[Array[Byte]]

				out.write(encoded)

			}
		}

	}

	def compareBytes(a: Array[Byte], b: Array[Byte]): Boolean = //Assumes equal length
	{
		var matches = true
		for (i <- a.indices)
		{
			if (a(i) != b(i))
				matches = false
		}

		matches
	}
}