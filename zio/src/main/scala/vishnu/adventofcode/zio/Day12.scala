package vishnu.adventofcode.zio

import zio._
import zio.logging._

import scala.collection.mutable

object Day12 extends AoCZIORunnable {

  sealed trait Direction extends Product with Serializable
  object Direction {
    case object North extends Direction
    case object South extends Direction
    case object East extends Direction
    case object West extends Direction
    case object Left extends Direction
    case object Right extends Direction
    case object Forward extends Direction

    def apply(dir: String): Direction = {
      dir match {
        case "N" => North
        case "S" => South
        case "E" => East
        case "W" => West
        case "L" => Left
        case "R" => Right
        case "F" => Forward
      }
    }

    implicit class DirectionChange(dir: Direction) {
      def change(degree: Int): Direction = {
        if (degree == 0) dir
        else {
          val newDir = dir match {
            case North => East
            case South => West
            case East  => South
            case West  => North
          }
          newDir.change(degree - 90)
        }
      }

    }
  }

  val inputRegex = "([NSEWLRF])(\\d+)".r

  final case class WayPoint(x: Int, y: Int) {
    def rotateClockwise(degree: Int): WayPoint =
      if (degree > 0) WayPoint(y, -x).rotateClockwise(degree - 90) else this
    def rotateCounterClockwise(degree: Int): WayPoint =
      if (degree > 0) WayPoint(-y, x).rotateCounterClockwise(degree - 90) else this
  }

  final case class Ship(x: Int, y: Int)

  def parseInput(str: String) = {
    val inputRegex(dirString, value) = str
    Direction(dirString) -> value.toInt
  }
  def collectTurns(input: Iterator[String]) = {
    def collect(str: String, mp: collection.mutable.Map[Direction, collection.mutable.Set[Int]]) = {
      parseInput(str) match {
        case (x @ (Direction.Left | Direction.Right), degree) =>
          mp.getOrElseUpdate(x, collection.mutable.Set[Int]()) += degree
        case _ =>
      }
    }

    for {
      mapRef <- Ref.make(collection.mutable.Map[Direction, mutable.Set[Int]]())
      mp <- mapRef.get
      _ <- ZIO.foreach_(Iterable.from(input))(str => ZIO.effectTotal(collect(str, mp)))
    } yield {
      mp
    }
  }

  def movePart2(
      ship: Ship,
      wayPoint: WayPoint,
      dir: Direction,
      value: Int
  ): (Ship, WayPoint) = {
    dir match {
      case Direction.North => (ship, WayPoint(wayPoint.x, wayPoint.y + value))
      case Direction.South => (ship, WayPoint(wayPoint.x, wayPoint.y - value))
      case Direction.East  => (ship, WayPoint(wayPoint.x + value, wayPoint.y))
      case Direction.West  => (ship, WayPoint(wayPoint.x - value, wayPoint.y))
      case Direction.Left  => (ship, wayPoint.rotateCounterClockwise(value))
      case Direction.Right => (ship, wayPoint.rotateClockwise(value))
      case Direction.Forward =>
        (Ship((wayPoint.x * value) + ship.x, (wayPoint.y * value) + ship.y), wayPoint)
    }
  }

  def movePart1(
      facingDir: Direction,
      x: Int,
      y: Int,
      direction: Direction,
      value: Int
  ): (Direction, Int, Int) = {
    direction match {
      case Direction.North => (facingDir, x, y + value)
      case Direction.South => (facingDir, x, y - value)
      case Direction.East  => (facingDir, x + value, y)
      case Direction.West  => (facingDir, x - value, y)
      case Direction.Left  => (facingDir.change(360 - value), x, y)
      case Direction.Right => (facingDir.change(value), x, y)
      case Direction.Forward =>
        facingDir match {
          case Direction.North => (facingDir, x, y + value)
          case Direction.South => (facingDir, x, y - value)
          case Direction.East  => (facingDir, x + value, y)
          case Direction.West  => (facingDir, x - value, y)
        }
    }
  }

  def moveZIOPart2(
      shipRef: Ref[Ship],
      wayPointRef: Ref[WayPoint],
      instruction: (Direction, Int)
  ) = {
    for {
      ship <- shipRef.get
      waypoint <- wayPointRef.get
      (newShip, newWayPoint) = movePart2(ship, waypoint, instruction._1, instruction._2)
      _ <- shipRef.set(newShip)
      _ <- wayPointRef.set(newWayPoint)
      _ <- log.info(s"${instruction}: ${newShip}, ${newWayPoint}")
    } yield {}
  }

  def navigate2(inputStrings: Seq[String]) = {
    val inputs = inputStrings.map(parseInput)
    for {
      shipRef <- Ref.make(Ship(0, 0))
      waypointRef <- Ref.make(WayPoint(10, 1))
      _ <- ZIO.foreach_(Iterable.from(inputs))(input => moveZIOPart2(shipRef, waypointRef, input))
      ship <- shipRef.get
      _ <- log.info(s"ship:x:${ship.x}")
      _ <- log.info(s"ship:y:${ship.y}")
    } yield {
      ship.x.abs + ship.y.abs
    }
  }

  def moveZIOPart1(
      dirRef: Ref[Direction],
      xRef: Ref[Int],
      yRef: Ref[Int],
      instruction: (Direction, Int)
  ): UIO[Unit] = {
    for {
      dir <- dirRef.get
      x <- xRef.get
      y <- yRef.get
      (newDir, newX, newY) = movePart1(dir, x, y, instruction._1, instruction._2)
      _ <- dirRef.set(newDir)
      _ <- xRef.set(newX)
      _ <- yRef.set(newY)
    } yield {}
  }

  def navigate1(inputString: Seq[String]) = {
    val inputs = inputString.map(parseInput)
    for {
      directionRef <- Ref.make(Direction.East: Direction)
      xRef <- Ref.make(0)
      yRef <- Ref.make(0)
      _ <- ZIO.foreach_(Iterable.from(inputs))(input =>
        moveZIOPart1(directionRef, xRef, yRef, input)
      )
      x <- xRef.get
      y <- yRef.get
      _ <- log.info(s"x:$x")
      _ <- log.info(s"y:$y")
    } yield {
      x.abs + y.abs
    }
  }

  override def getRunLayer(input: Iterator[String]) = {
    val inputSeq = input.toSeq
    for {
      relPart1 <- navigate1(inputSeq)
      _ <- console.putStrLn(relPart1.toString)
      relPart2 <- navigate2(inputSeq)
      _ <- console.putStrLn(relPart2.toString)
    } yield {}
  }

//  override val resourceName: String = "/day12_sample.txt"
  override val resourceName: String = "/day12_input.txt"
}
