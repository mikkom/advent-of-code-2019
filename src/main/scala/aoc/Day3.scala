package aoc

import cats.implicits._

import scala.io.Source

sealed trait Direction
object Direction {
  case object Up extends Direction
  case object Down extends Direction
  case object Right extends Direction
  case object Left extends Direction
}

case class Move(dir: Direction, count: Int)

object Wires {
  case class Coord(x: Int, y: Int)

  def handleDir(dir: Direction, n: Int, origin: Coord) = {
    dir match {
      case Direction.Right => origin.copy(x = origin.x + n)
      case Direction.Left  => origin.copy(x = origin.x - n)
      case Direction.Up    => origin.copy(y = origin.y + n)
      case Direction.Down  => origin.copy(y = origin.y - n)
    }
  }

  def drawMoves(
      moves: List[Move],
      canvas: Map[Coord, Int] = Map.empty,
      origin: Coord = Coord(0, 0),
      step: Int = 0
  ): Map[Coord, Int] = moves match {
    case Nil => canvas
    case move :: rest => {
      val newCoords =
        (1 to move.count).toList
          .map(n => (handleDir(move.dir, n, origin), step + n))
      drawMoves(rest, canvas ++ newCoords, newCoords.last._1, step + move.count)
    }
  }

  def answerPart1(wire1: List[Move], wire2: List[Move]) = {
    val intersectionPoints = drawMoves(wire1).keySet intersect drawMoves(wire2).keySet
    intersectionPoints.map(c => Math.abs(c.x) + Math.abs(c.y)).min
  }

  def answerPart2(wire1: List[Move], wire2: List[Move]) = {
    val moves1 = drawMoves(wire1)
    val moves2 = drawMoves(wire2)
    val intersectionPoints = moves1.keySet intersect moves2.keySet
    intersectionPoints.map(coord => moves1(coord) + moves2(coord)).min
  }
}

object Day3 extends App {
  import Wires._

  def parseDir(ch: Char) = ch match {
    case 'R' => Direction.Right
    case 'L' => Direction.Left
    case 'U' => Direction.Up
    case 'D' => Direction.Down
  }

  def parseMove(str: String) = {
    Move(parseDir(str(0)), str.substring(1).toInt)
  }

  val input = Source
    .fromResource("input-day3.txt")
    .getLines()
    .toList
    .map(line => line.split(',').map(parseMove).toList)

  val wire1 = input(0)
  val wire2 = input(1)

  println("Day 3 pt 1: " + answerPart1(wire1, wire2))
  println("Day 3 pt 2: " + answerPart2(wire1, wire2))
}
