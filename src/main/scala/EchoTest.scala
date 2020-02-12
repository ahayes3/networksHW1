import java.nio.ByteBuffer

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object EchoTest
{
	def main(args:Array[String]) //LIST BUFFERS
	{
		val echoPort = 7
		val host = "gee.cs.oswego.edu"

		var times = new Array[(Long,Long)](50)

		println("Enter buffer size.")
		val bufferSize = StdIn.readInt()
		val buffer = ByteBuffer.allocateDirect(bufferSize)


	}
}