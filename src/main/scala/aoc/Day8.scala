package aoc.day8

import scala.io.Source
import cats.implicits._
import cats.Semigroup
import aoc.Parsing

sealed trait Color

object Color {
  case object Black extends Color
  case object White extends Color
  case object Transparent extends Color

  implicit def semigroupForColor = new Semigroup[Color] {
    override def combine(x: Color, y: Color): Color = (x, y) match {
      case (Transparent, _) => y
      case _                => x
    }
  }

  val fromInt = Map(0 -> Black, 1 -> White, 2 -> Transparent)
}

case class Layer(pixels: List[Color])

object Layer {
  implicit val semigroupForLayer = new Semigroup[Layer] {
    override def combine(l1: Layer, l2: Layer): Layer =
      Layer((l1.pixels zip l2.pixels).map {
        case (x, y) => x |+| y
      })
  }
}

object SpaceImageFormat {
  def answerPart1(pixels: List[Int], width: Int, height: Int): Int = {
    val pixelCount = width * height
    val layers = pixels.sliding(pixelCount, pixelCount)
    val layer = layers.minBy(_.filter(_ == 0).length)
    layer.filter(_ == 1).length * layer.filter(_ == 2).length
  }

  def renderLayer(layer: Layer, width: Int): String = {
    layer.pixels
      .map {
        case Color.Black       => "0"
        case Color.White       => "1"
        case Color.Transparent => "2"
      }
      .sliding(width, width)
      .map(row => row.mkString)
      .mkString("\n")
  }

  def answerPart2(pixels: List[Int], width: Int, height: Int): String = {
    val pixelCount = width * height
    val layers = pixels
      .traverse(Color.fromInt.get(_))
      .get
      .sliding(pixelCount, pixelCount)
      .map(Layer(_))

    val combined = Semigroup[Layer].combineAllOption(layers)
    renderLayer(combined.get, width)
  }
}

object Main extends App {
  import SpaceImageFormat._

  val input = Source.fromResource("input-day8.txt").getLines().toList.head
  val digits = Parsing.toDigits(input)

  println("Day 8 pt 1: " + answerPart1(digits, 25, 6))
  println("Day 8 pt 2:\n" + answerPart2(digits, 25, 6))
}
