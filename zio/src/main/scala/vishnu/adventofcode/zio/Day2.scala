package vishnu.adventofcode.zio

import zio._

object Day2 extends AoCZIORunnable {

  def countValidInputs(inputs:Iterator[String]):UIO[Int] = {
    ZIO.effectTotal(inputs.count(vishnu.adventofcode.justscala.Day2.isInputValid))
  }

  def countTobogganValidInputs(inputs:Iterator[String]):UIO[Int] = {
    ZIO.effectTotal(inputs.count(vishnu.adventofcode.justscala.Day2.tobogganValid))
  }


  override def getRunLayer(input: Iterator[String]) = {
    for {
      validCount <- readResource(resourceName).use(countValidInputs)
      tobogganValidCount <- readResource(resourceName).use(countTobogganValidInputs)
      _ <- console.putStrLn(s"Valid:$validCount; TobogganValid:$tobogganValidCount")
    } yield {

    }
  }

  //    val resourceName = "/day2_sample.txt"
  val resourceName = "/day2_input.txt"
}