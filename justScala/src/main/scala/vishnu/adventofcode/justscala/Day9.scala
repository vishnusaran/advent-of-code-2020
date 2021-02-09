package vishnu.adventofcode.justscala

object Day9 {
  def findBrokenLink(data: Iterator[Long], n:Int) = {
    var queue = scala.collection.mutable.Queue[Long]()
    val set = scala.collection.mutable.Set[Long]()
    val preambleData = data.take(n).toList
    queue ++= preambleData
    set ++= preambleData
    val reminingData = data.dropWhile { i =>
      if(queue.exists( a => set.contains(i - a))){
        val elem = queue.dequeue()
        set -= elem
        set += i
        queue += i
        true
      } else {
        false
      }
    }
    reminingData.next()
  }

  def findWeakness(data:Iterator[Long], brokenValue:Long) = {
    val weakData = data.takeWhile(_ != brokenValue)
    val queue = scala.collection.mutable.Queue[Long]()
    var sum = 0L
    weakData.exists{ a =>
      queue += a
      sum += a
      while (sum > brokenValue) {
        sum -= queue.dequeue()
      }
      sum == brokenValue
    }
    println(queue.toList)
    queue.min.toLong + queue.max.toLong
  }



  def main(args: Array[String]): Unit = {
//    val stream = this.getClass.getResourceAsStream("/day9_sample.txt")
//    val preambleSize = 5;

    val stream = this.getClass.getResourceAsStream("/day9_input.txt")
    val preambleSize = 25;

    val dataLines = scala.io.Source.fromInputStream(stream).getLines().toList
    val data = dataLines.map(_.toLong)
    val brokenLink = findBrokenLink(data.iterator, preambleSize)
    println(brokenLink)
    println(findWeakness(data.iterator, brokenLink))
  }
}
