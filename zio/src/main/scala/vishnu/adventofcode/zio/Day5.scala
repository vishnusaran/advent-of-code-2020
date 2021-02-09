package vishnu.adventofcode.zio

import zio._
import zio.logging._

object Day5 extends AoCZIORunnable {




  override def getRunLayer(input: Iterator[String]) = {
    val seatIds = input.map(vishnu.adventofcode.justscala.Day5.getSeatId).toSeq.sorted
    val (l,_) = (seatIds zip seatIds.tail).find{ case (l,r)=>
      l+2 == r
    }.get

    console.putStrLn(s"Max:${seatIds.max}, mySeat:${l+1}")
  }

  override val resourceName: String = "/day5_input.txt"
}