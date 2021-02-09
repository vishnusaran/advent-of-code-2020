package vishnu.adventofcode.zio

import zio._

object Day6 extends AoCZIORunnable {

  def runCount(input:Iterator[String], countFunction:(Iterator[String]=> Int)):UIO[Int] = {
    if(input.isEmpty){
      ZIO.succeed(0)
    } else {
      val count = countFunction(input)
      runCount(input, countFunction).map(_ + count)
    }
  }

  override def getRunLayer(input: Iterator[String]) = {
    val inputList = input.toList
    for {
      part1 <- runCount(inputList.iterator, vishnu.adventofcode.justscala.Day6.countAnyAnswersInGroup)
      part2 <- runCount(inputList.iterator, vishnu.adventofcode.justscala.Day6.countMatchingAnswersInGroup)
      _ <- console.putStrLn(s"part1:$part1, part2:$part2")
    } yield {

    }
  }

//  override val resourceName: String = "/day6_sample.txt"
  override val resourceName: String = "/day6_input.txt"
}
