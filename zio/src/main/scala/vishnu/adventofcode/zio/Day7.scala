package vishnu.adventofcode.zio

import zio._

import scala.collection.mutable

object Day7 extends AoCZIORunnable {
  def dfsToTarget(graph: Map[String, Seq[(String, Int)]], target: String) = {

    def localDfs(start: String)(
        implicit visitedRef: Ref[mutable.Set[String]],
        canReachRef: Ref[mutable.Set[String]]
    ): UIO[Boolean] = {
      for {
        visited <- visitedRef.get
        canReach <- canReachRef.get
        rel <- {
          if (visited.contains(start)) ZIO.effectTotal(canReach.contains(start))
          else {
            visitedRef.set(visited += start) *> {
              if (graph(start).map(_._1).contains(target))
                canReachRef.set(canReach += start) *> ZIO.succeed(true)
              else
                ZIO
                  .foreach(graph(start))({
                    case (neigh, _) => localDfs(neigh)(visitedRef, canReachRef)
                  })
                  .map(a => a.contains(true))
            }
          }
        }
        _ <- ZIO.when(rel)(canReachRef.set(canReach += start))
      } yield {
        rel
      }
    }

    for {
      canReachRef <- Ref.make(mutable.Set[String]())
      visitedRef <- Ref.make(mutable.Set[String]())
      _ <- ZIO.foreach_(graph.keySet.filterNot(_ == target))(node =>
        localDfs(node)(visitedRef, canReachRef)
      )
      canReachSet <- canReachRef.get
    } yield {
      canReachSet.size
    }
  }

  def bfsFromSource(
      graph: Map[String, Seq[(String, Int)]],
      source: String
  ): ZIO[Any, Nothing, Int] = {
    def localBfs(
        queue: zio.Queue[(String, Int)],
        counter: Ref[Int]
    ): ZIO[Any, Nothing, Unit] = {
      for {
        elem <- queue.poll
        _ <- elem match {
          case Some((node, weight)) =>
            ZIO.foreach_(graph(node)) {
              case (neigh, neighWeight) =>
                counter.update(_ + (weight * neighWeight)) *>
                  queue.offer((neigh, weight * neighWeight))
            } *> localBfs(queue, counter)
          case None => ZIO.unit
        }
      } yield {}

    }
    for {
      q <- Queue.unbounded[(String, Int)]
      counter <- Ref.make(0)
      _ <- q.offer((source, 1))
      _ <- localBfs(q, counter)
      count <- counter.get
    } yield {
      count
    }
  }

  override def getRunLayer(input: Iterator[String]) = {
    val graph = input.map(vishnu.adventofcode.justscala.Day7.parseInput).toMap
    for {
      dfsCount <- dfsToTarget(graph, "shiny gold")
      _ <- console.putStrLn(s"To shiny gold:$dfsCount")
      bfsCount <- bfsFromSource(graph, "shiny gold")
      _ <- console.putStrLn(s"From shiny gold:$bfsCount")
    } yield {}
  }

//  override val resourceName: String = "/day7_sample.txt"
  override val resourceName: String = "/day7_input.txt"
}
