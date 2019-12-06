package aoc

import cats.implicits._

import scala.io.Source

object Passcode {
  def containsDouble(n: Int): Boolean = {
    val doubles = (0 to 9).map(n => s"$n$n")
    val str = String.format("%06d", n)
    doubles.exists(str.contains(_))
  }

  def containsStrictDouble(n: Int): Boolean = {
    val doubles = (0 to 9).map(n => s"$n$n")
    val triples = (0 to 9).map(n => s"$n$n$n")
    val str = String.format("%06d", n)
    doubles.exists(str.contains(_)) && triples.forall(!str.contains(_))
  }

  def getPasswordCount(
      minValue: Int,
      maxValue: Int,
      criteria: Int => Boolean
  ) = {
    val passwords = for {
      d1 <- 0 to 9
      d2 <- d1 to 9
      d3 <- d2 to 9
      d4 <- d3 to 9
      d5 <- d4 to 9
      d6 <- d5 to 9
      value = List(d1, d2, d3, d4, d5, d6).mkString.toInt
      if value >= minValue && value <= maxValue && criteria(value)
    } yield value

    (passwords.toList.take(20), passwords.length)
  }

  def answerPart1(minValue: Int, maxValue: Int) = {
    getPasswordCount(minValue, maxValue, containsDouble(_))
  }

  def answerPart2(minValue: Int, maxValue: Int) = {
    getPasswordCount(minValue, maxValue, containsStrictDouble(_))
  }
}

object Day4 extends App {
  import Passcode._

  val Array(minValue, maxValue) = Source
    .fromResource("input-day4.txt")
    .getLines()
    .toList
    .head
    .split("-")
    .map(_.toInt)

  println("Day 4 pt 1: " + answerPart1(minValue, maxValue))
  println("Day 4 pt 2: " + answerPart2(minValue, maxValue))
}
