package aoc

import cats.{Monoid, Order}
import cats.implicits._

import scala.io.Source

case class Mass(value: Int)
object Mass {
  val ZERO: Mass = Mass(0)

  implicit val monoidForMass: Monoid[Mass] = MonoidDerivation.gen[Mass]
  implicit val orderForMass: Order[Mass] = (x: Mass, y: Mass) =>
    x.value.compare(y.value)
}

object FuelCalculations {
  def requiredFuel(mass: Mass) = Mass(mass.value / 3 - 2)

  def totalRequiredFuel(mass: Mass) =
    Iteration
      .iterate(mass)(requiredFuel)
      .takeWhile(_ > Mass.ZERO)
      .combineAll

  def answerPart1(masses: List[Mass]) = masses.map(requiredFuel).combineAll

  def answerPart2(masses: List[Mass]) = masses.map(totalRequiredFuel).combineAll
}

object Day1 extends App {
  import FuelCalculations._

  val input = Source.fromResource("input-day1.txt").getLines().toList
  val masses = input.map(str => Mass(str.toInt))

  println("Day 1 pt 1: " + answerPart1(masses).value)
  println("Day 1 pt 2: " + answerPart2(masses).value)
}
