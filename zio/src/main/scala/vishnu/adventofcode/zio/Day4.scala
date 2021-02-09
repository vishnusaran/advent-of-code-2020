package vishnu.adventofcode.zio

import zio._

object Day4 extends AoCZIORunnable {
  type Passport = Map[String, String]

  def getAllPassports(input:Iterator[String]):UIO[List[Passport]] = {
    if(input.isEmpty){
      ZIO.succeed(Nil)
    } else {
      for {
        passport <- ZIO.effectTotal(vishnu.adventofcode.justscala.Day4.getAPassport(input))
        rest <- getAllPassports(input)
      } yield {
        passport :: rest
      }
    }
  }


//  override val resourceName: String = "/day4_sample.txt"
  override val resourceName: String = "/day4_input.txt"

  override def getRunLayer(input: Iterator[String]) = {
    for {
      passports <- getAllPassports(input)
      validCount <- ZIO.effectTotal(passports.count(vishnu.adventofcode.justscala.Day4.validatePassport))
      throughValidCount <- ZIO.effectTotal(passports.count(vishnu.adventofcode.justscala.Day4.throughValidatePassport))
      _ <- console.putStrLn(s"valid:$validCount; throughValid:$throughValidCount")
    } yield {

    }
  }
}