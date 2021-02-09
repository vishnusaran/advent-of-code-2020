package vishnu.adventofcode.justscala



object Day10 {
  def findDistribution(adopters:Seq[Int]) = {
    val mp = scala.collection.mutable.Map[Int, Int](1 -> 0, 2 -> 0, 3 -> 0)
    var joltage = 0
    adopters.sorted.foreach{ currentJoltage =>
      val jump = currentJoltage - joltage
      joltage = currentJoltage
      mp += jump -> (mp(jump) + 1)
    }
    println(mp)
    mp(1) * (mp(3)+1)
  }

  def distinctArrangements(adopters:Seq[Int]):Long = {
    val paddedAdopters = (0 +: adopters.sorted :+ (adopters.max + 3)).toArray
    val countArr = Array.ofDim[Long](paddedAdopters.length)
    countArr(paddedAdopters.length -1) = 1
    var i = paddedAdopters.length - 2
    while (i >= 0) {
      var ways = 0L
      if(paddedAdopters(i+1) - paddedAdopters(i) <=3){
        ways += countArr(i+1)
      }
      if( i+2 < paddedAdopters.length && paddedAdopters(i+2) - paddedAdopters(i) <=3){
        ways += countArr(i+2)
      }
      if( i+3 < paddedAdopters.length && paddedAdopters(i+3) - paddedAdopters(i) <= 3){
        ways += countArr(i+3)
      }
      countArr(i) = ways
      i -= 1
    }
    countArr(0)
  }

  def main(args: Array[String]): Unit = {
//    val stream = this.getClass.getResourceAsStream("/day10_simple_sample.txt")
//    val stream = this.getClass.getResourceAsStream("/day10_sample.txt")
    val stream = this.getClass.getResourceAsStream("/day10_input.txt")
    val adopters = scala.io.Source.fromInputStream(stream).getLines().map(_.toInt).toSeq
//    println(findDistribution(adopters))
    println(distinctArrangements(adopters))

  }
}
