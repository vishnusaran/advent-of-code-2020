package vishnu.adventofcode.justscala

object Day5 {
  def getSeatId(str:String) = {
    val row = str.take(7).foldLeft((0, 127)) { case ((l, r), c) =>
      val mid = l + ((r - l) / 2)
      c match {
        case 'F' => (l, mid)
        case 'B' => (mid + 1, r)
      }
    }._1

    val col = str.drop(7).foldLeft((0,7)){ case ((l,r),c)=>
      val mid = l + ((r - l) / 2)
      c match {
        case 'L' => (l,mid)
        case 'R' => (mid+1,r)
      }
    }._1

    row * 8 + col
  }

  def main(args: Array[String]): Unit = {
    println(getSeatId("BBFFBBFRLL"))
    val inputStream = this.getClass.getResourceAsStream("/seats.txt")
    val seatIds = scala.io.Source.fromInputStream(inputStream).getLines().map(getSeatId).toList.sorted
    val max = seatIds.max
    println(s"${max}")

    val (l,r) = (seatIds zip seatIds.tail).find{ case (l,r)=>
      l+2 == r
    }.get

    println(l,r,l+1)

    
  }
}
