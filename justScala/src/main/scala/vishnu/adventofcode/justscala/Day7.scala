package vishnu.adventofcode.justscala

object Day7 {
  val bagColorRegex = "(.*) bags?".r
  val countedColorRegex = "(\\d+) (.*) bags?\\.?".r

  def parseInput(str: String): (String, Seq[(String, Int)]) = {
    val mapping = str.split("contain").map(_.trim)
    val bagColorRegex(sourceColor) = mapping(0)
    val target = if (mapping(1).trim == "no other bags.") {
      Seq.empty[(String,Int)]
    } else {
      mapping(1).split(",").map(_.trim).map { tgt =>
        val countedColorRegex(count, targetColor) = tgt
        (targetColor, count.toInt)
      }.toSeq
    }
    sourceColor -> target
  }

  def dfsToTarget(graph:Map[String,Seq[(String,Int)]], target:String) = {
    val visitedMap = scala.collection.mutable.Map[String,Boolean]()
    val canReachTarget = scala.collection.mutable.Set[String]()

    def dfsRec(node:String): Boolean = {
      if(!visitedMap.contains(node)){
        val dfsStatus = graph(node).find{ case (neighbour,_) =>
          if(neighbour == target){
            true
          } else {
            dfsRec(neighbour)
          }
        }
        if(dfsStatus.isDefined){
          canReachTarget.add(node)
        }
        dfsStatus.isDefined
      } else {
        canReachTarget.contains(node)
      }
    }

    graph.keySet.filterNot(_ == target).foreach(dfsRec)
    canReachTarget.size
  }

  def bfsFromSource(graph:Map[String,Seq[(String,Int)]], source:String) = {
    val q = scala.collection.mutable.Queue[(String,Int)]()
    var count = 0;
    q += (source->1)
    while(q.nonEmpty){
      val (node, weight) = q.dequeue()
      graph(node).foreach{ case (neigh,neighWeight) =>
        count += weight * neighWeight
        q += (neigh -> (weight * neighWeight))
      }
    }
    count
  }


  def main(args: Array[String]): Unit = {
//    val inputStream = this.getClass.getResourceAsStream("/day7_sample.txt")
    val inputStream = this.getClass.getResourceAsStream("/day7_input.txt")
    val input = scala.io.Source.fromInputStream(inputStream).getLines()
    val graph: Map[String, Seq[(String, Int)]] = input.map(parseInput).toMap
//    graph.map{case (k,v) => s"$k -> $v"}.foreach(println)
    println(dfsToTarget(graph, "shiny gold"))
    println(bfsFromSource(graph, "shiny gold"))
  }
}
