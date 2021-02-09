package vishnu.adventofcode.zio
import zio._
import zio.logging.Logging


object Day3 extends AoCZIORunnable {
//  override def getBluePrint(args: List[String]) = {
//    val resourceName = "/day3_input.txt"
//    for {
//      input <- readResource(resourceName).map(_.toArray).
//      ???
//    } yield {
//
//    }
//  }
  override val resourceName: String = "/day3_input.txt"

  def getSlopes(slope:Array[String],paths:Iterator[(Int,Int)]):UIO[Long] = {
    if(paths.isEmpty){
      ZIO.succeed(1L)
    } else {
      val (right, down) = paths.next()
      for {
        trej <- ZIO.effectTotal(vishnu.adventofcode.justscala.Day3.getTobogganTrajectory(slope,right,down))
        next <- getSlopes(slope, paths)
      } yield {
        trej * next
      }
    }
  }

  override def getRunLayer(input: Iterator[String]): ZIO[zio.ZEnv with Logging, Any, Unit] = {
    val arrayInput = input.toArray
    val paths = List((1,1), (3,1), (5,1),(7,1),(1,2))
    for {
      rel <- getSlopes(arrayInput, paths.iterator)
      _ <- console.putStrLn(rel.toString)
    } yield {

    }

  }
}