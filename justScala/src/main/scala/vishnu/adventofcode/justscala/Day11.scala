package vishnu.adventofcode.justscala

object Day11 {

  val occupied = '#'
  val openSeat = 'L'

  final case class Layout(board: Array[Array[Char]]) {
    val north = Array.ofDim[Boolean](board.length, board(0).length)
    val south = Array.ofDim[Boolean](board.length, board(0).length)
    val east = Array.ofDim[Boolean](board.length, board(0).length)
    val west = Array.ofDim[Boolean](board.length, board(0).length)
    val northEast = Array.ofDim[Boolean](board.length, board(0).length)
    val northWest = Array.ofDim[Boolean](board.length, board(0).length)
    val southEast = Array.ofDim[Boolean](board.length, board(0).length)
    val southWest = Array.ofDim[Boolean](board.length, board(0).length)

    def checkAndGetValue(i: Int, j: Int, cache: Array[Array[Boolean]]) = {
      if (board(i)(j) == '.') {
        cache(i)(j)
      } else {
        board(i)(j) == occupied
      }
    }

    for (i <- board.indices) {
      for (j <- board(i).indices) {
        if (i > 0) { // north
          north(i)(j) = checkAndGetValue(i - 1, j, north)
        }
        if (j > 0) { //west
          west(i)(j) = checkAndGetValue(i, j - 1, west)
        }
        if (i > 0 && j > 0) {
          northWest(i)(j) = checkAndGetValue(i - 1, j - 1, northWest)
        }
        if (i > 0 && j < board(i).length - 1) {
          northEast(i)(j) = checkAndGetValue(i - 1, j + 1, northEast)
        }
      }
    }

    for (i <- board.indices.reverse) {
      for (j <- board(i).indices.reverse) {
        if (i < board.length - 1) {
          south(i)(j) = checkAndGetValue(i + 1, j, south)
        }
        if (j < board(i).length - 1) {
          east(i)(j) = checkAndGetValue(i, j + 1, east)
        }
        if (i < board.length - 1 && j < board(i).length - 1) {
          southEast(i)(j) = checkAndGetValue(i + 1, j + 1, southEast)
        }
        if (i < board.length - 1 && j > 0) {
          southWest(i)(j) = checkAndGetValue(i + 1, j - 1, southWest)
        }
      }
    }

    def countOccupied(x: Int, y: Int) = {
      var count = 0
      if (north(x)(y)) count += 1
      if (west(x)(y)) count += 1
      if (east(x)(y)) count += 1
      if (south(x)(y)) count += 1
      if (northWest(x)(y)) count += 1
      if (northEast(x)(y)) count += 1
      if (southWest(x)(y)) count += 1
      if (southEast(x)(y)) count += 1
      count
    }

    override def toString: String = {
      board.map(row => row.mkString).mkString("\n")
    }
  }

  def changeSeatsOptimized(layout: Layout) = {
    val newBoard = Array.tabulate(layout.board.length, layout.board(0).length) { (i, j) =>
      layout.board(i)(j)
    }
    for (i <- layout.board.indices) {
      for (j <- layout.board(i).indices) {
        val occupiedAround = layout.countOccupied(i, j)
        layout.board(i)(j) match {
          case 'L' if occupiedAround == 0 => newBoard(i)(j) = occupied
          case '#' if occupiedAround >= 5 => newBoard(i)(j) = openSeat
          case _                          => //Do nothing
        }
      }
    }
    Layout(newBoard)
  }

  def changeSeats(layout: Array[Array[Char]]) = {
    val newLayout = Array.tabulate(layout.length, layout(0).length) { (i, j) => layout(i)(j) }
    for {
      i <- layout.indices
      j <- layout(i).indices
    } yield {
      val occupiedAround = countOccupied(i, j)(layout)

      layout(i)(j) match {
        case 'L' if occupiedAround == 0 => newLayout(i)(j) = '#'
        case '#' if occupiedAround >= 4 => newLayout(i)(j) = 'L'
        case _                          => //Do nothing
      }
    }
    newLayout
  }

  def countOccupied(x: Int, y: Int)(
      implicit layout: Array[Array[Char]]
  ): Int = {
    var count = 0;
    if (x > 0) {
      if (y > 0 && layout(x - 1)(y - 1) == '#') count += 1
      if (layout(x - 1)(y) == '#') count += 1
      if ((y < layout(x).length - 1) && layout(x - 1)(y + 1) == '#') count += 1
    }
    if (y > 0 && layout(x)(y - 1) == '#') count += 1
    if ((y < layout(x).length - 1) && layout(x)(y + 1) == '#') count += 1

    if (x < layout.length - 1) {
      if (y > 0 && layout(x + 1)(y - 1) == '#') count += 1
      if (layout(x + 1)(y) == '#') count += 1
      if ((y < layout(x).length - 1) && layout(x + 1)(y + 1) == '#') count += 1
    }
    count
  }

  def printLayout(layout: Array[Array[Char]]) = {
    layout.foreach { row => println(row.mkString("")) }
  }

  def changeSeatUntilNoChange[T](
      layout: T,
      changeFunction: T => T,
      boardExtractor: T => Array[Array[Char]]
  ) = {
    var currentLayout = layout
    var lastLayout = layout
    var count = 0
    println(layout)
    do {
      count += 1
      println(s"round $count")
      lastLayout = currentLayout
      currentLayout = changeFunction(currentLayout)
//      println(currentLayout)
    } while (!(layoutEquals(boardExtractor(currentLayout), boardExtractor(lastLayout))))
    currentLayout
  }
//  def changeSeatUntilNoChange(layout: Array[Array[Char]]) = {
//    var currentLayout = layout
//    var lastLayout = layout
//    var count = 0
//    do {
//      println({
//        count += 1; s"round $count"
//      })
//      lastLayout = currentLayout
//      currentLayout = changeSeats(currentLayout)
////      printLayout(currentLayout)
//    } while (!(layoutEquals(currentLayout, lastLayout)))
//    currentLayout
//  }

  def layoutEquals(left: Array[Array[Char]], right: Array[Array[Char]]) = {
    left.flatten.mkString == right.flatten.mkString
  }
  def countOccupiedSeats(layout: Array[Array[Char]]) =
    layout.iterator.flatten.count(_ == '#')

  def main(args: Array[String]): Unit = {
//    val stream = this.getClass.getResourceAsStream("/day11_sample.txt")
    val stream = this.getClass.getResourceAsStream("/day11_input.txt")

    val layout =
      scala.io.Source.fromInputStream(stream).getLines().map(_.toArray).toArray
//    val newLayout = changeSeatUntilNoChange(layout, changeSeats, identity[Array[Array[Char]]])
//    println(countOccupiedSeats(newLayout))

    val newLayout = changeSeatUntilNoChange[Layout](Layout(layout), changeSeatsOptimized, _.board)
    println(countOccupiedSeats(newLayout.board))

  }
}
