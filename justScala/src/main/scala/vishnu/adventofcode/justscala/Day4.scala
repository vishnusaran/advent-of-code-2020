package vishnu.adventofcode.justscala

import scala.util.Try

object Day4 {

  val heightRegex = "(\\d+)(cm|in)".r
  val hairColorRegex = "^#[a-z0-9]{6}$".r
  val eyeColorRegex = "^(amb|blu|brn|gry|grn|hzl|oth)$".r
  val passportIdRegex = "^\\d{9}$".r

  def getAPassport(lines: Iterator[String]): Map[String, String] = {
    lines
            .takeWhile(_.nonEmpty)
            .flatMap(_.split(" "))
            .map(str => str.split(":"))
            .map(pair => pair(0) -> pair(1))
            .toMap
  }

  def validatePassport(passport: Map[String, String]) = {
    passport.size == 8 || (passport.size == 7 && !passport.contains("cid"))
  }

  def throughValidatePassport(passport: Map[String, String]) = {
    Try(passport)
            .filter(validatePassport)
            .filter { passport => val value = passport("byr").toInt; value >= 1920 && value <= 2002 }
            .filter { passport => val value = passport("iyr").toInt; value >= 2010 && value <= 2020 }
            .filter { passport => val value = passport("eyr").toInt; value >= 2020 && value <= 2030 }
            .filter { passport =>
              val heightRegex(height, measure) = passport("hgt");
              (measure == "in" && height.toInt >= 59 && height.toInt <= 76) ||
                      (measure == "cm" && height.toInt >= 150 && height.toInt <= 193)
            }
            .filter(passport => hairColorRegex.pattern.matcher(passport("hcl")).matches())
            .filter(passport => eyeColorRegex.pattern.matcher(passport("ecl")).matches())
            .filter(passport => passportIdRegex.pattern.matcher(passport("pid")).matches())
            .toOption.isDefined
  }

  def main(args: Array[String]): Unit = {

    val input = scala.io.Source.fromFile("/tmp/passportEntries.txt").getLines()
    var validPassports = 0
    while (input.hasNext) {
      val passport = getAPassport(input)
      if (throughValidatePassport(passport))
        validPassports += 1
    }
    println(validPassports)
  }


}
