package vishnu.adventofcode.justscala

object Day3 {
  def getTobogganTrajectory(slope:Array[String],right:Int, down:Int)= {
    var rowPos = 0
    var y = 0
    var counter = 0
    while(rowPos < slope.length){
      if(slope(rowPos)(y) == '#'){
        counter +=1
      }
      y = (y+right) % slope(0).length
      rowPos += down
    }
    counter
  }

  def main(args: Array[String]): Unit = {
//        val land = Array(
//          "..##.........##.........##.........##.........##.........##.......",
//          "#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..",
//          ".#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.",
//          "..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#",
//          ".#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.",
//          "..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....",
//          ".#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#",
//          ".#........#.#........#.#........#.#........#.#........#.#........#",
//          "#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...",
//          "#...##....##...##....##...##....##...##....##...##....##...##....#",
//          ".#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#")
    val land = scala.io.Source.fromFile("/tmp/land.txt").getLines().toArray
    val path  = List(
      (1,1),
      (3,1),
      (5,1),
      (7,1),
      (1,2)
    )
    println((path.foldLeft(1L) { case (mul, (right, down)) => mul * getTobogganTrajectory(land, right, down) }))


//    println(getTobogganTrajectory(land,3,1))
  }
}
