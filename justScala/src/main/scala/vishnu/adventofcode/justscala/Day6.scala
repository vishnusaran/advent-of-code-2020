package vishnu.adventofcode.justscala

object Day6 {
  def countAnyAnswersInGroup(answers: Iterator[String]): Int = {
    val boolArray: Array[Boolean] = Array.ofDim[Boolean](26)

    answers.takeWhile(_.nonEmpty).foreach { str =>
      str.foreach(c => boolArray(c - 'a') = true)
    }
    boolArray.count(identity)
  }

  def countMatchingAnswersInGroup(answers: Iterator[String]) = {
    val intArr = Array.ofDim[Int](26)
    var memberCount = 0
    answers.takeWhile(_.nonEmpty).foreach { str =>
      memberCount += 1
      str.foreach(c => intArr(c - 'a') = 1 + intArr(c - 'a'))
    }
    intArr.count(_ == memberCount)
  }

  def main(args: Array[String]): Unit = {
    val iterator = List("abc",
      "",
      "a",
      "b",
      "c",
      "",
      "ab",
      "ac",
      "",
      "a",
      "a",
      "a",
      "a",
      "",
      "b").iterator


    val inputStream = this.getClass.getResourceAsStream("/group_answers.txt")
    val lines = scala.io.Source.fromInputStream(inputStream).getLines()

    var counter = 0
    val itr = lines
    while (itr.hasNext) {
      counter += countAnyAnswersInGroup(lines)
//      counter += countMatchingAnswersInGroup(itr)
    }
    println(counter)


  }
}
