package vishnu.adventofcode.zio

import zio._
import zio.logging._

object Day8 extends AoCZIORunnable {
  override val resourceName: String = "/day8_sample.txt"

  override def getRunLayer(input: Iterator[String]) = {
    val instructions: Array[(String, Int)] =
      input.map(vishnu.adventofcode.justscala.Day8.parse).toArray
    ???

  }

}
