package vishnu.adventofcode.zio

import zio._
import zio.logging._

import scala.collection.mutable

object Day1 extends AoCZIORunnable {
  def findTarget(l: Ref[mutable.Set[Int]], target: Int) = {
    for {
      set <- l.get
      _ <- log.debug(s"looking for $target in set $l")
    } yield {
      set.contains(target)
    }
  }

  def find(target: Int, nums: List[Int], setRef: Ref[mutable.Set[Int]]): URIO[Logging, Option[Int]] = {
    if (nums.isEmpty) {
      ZIO.none
    } else {
      for {
        set <- setRef.get
        head = nums.head
        _ <- setRef.set(set -= head)
        found <- findTarget(setRef, target - head)
        rel <- if (found) {
          ZIO.some(head * (target - head))
        } else find(target, nums.tail, setRef)
      } yield {
        rel
      }
    }
  }

  def findTriples(target: Int, nums: List[Int], setRef: Ref[mutable.Set[Int]]): URIO[Logging, Option[Long]] = {
    if (nums.sizeIs < 2) {
      ZIO.none
    } else {
      for {
        set <- setRef.get
        head = nums.head
        _ <- setRef.set(set -= head)
        found <- find(target - head, nums.tail, setRef)
        rel <- found match {
          case Some(value) => ZIO.some(value.toLong * head)
          case None => setRef.set(set ++= nums.tail) *> findTriples(target, nums.tail, setRef)
        }
      } yield {
        rel
      }
    }
  }


  override def getRunLayer(input: Iterator[String]) = {
    def bluePrint(tgt: Int, lines: Iterator[String]) = {
      val nums = lines.map(_.toInt).toList
      for {
        numsSet <- Ref.make(scala.collection.mutable.Set(nums: _*))
        sum2 <- find(tgt, nums, numsSet)
        _ <- console.putStrLn(s"2Sum:${sum2}")
        sum3 <- findTriples(tgt, nums, numsSet)
        _ <- console.putStrLn(s"3Sum:$sum3")
      } yield {

      }
    }

    val tgt = 2020
    bluePrint(tgt, input)

  }

  //    val resourceName = "/day1_sample.txt"
  val resourceName = "/day1_input.txt"

}
