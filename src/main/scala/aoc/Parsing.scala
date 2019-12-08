package aoc

object Parsing {
  def toDigits(str: String): List[Int] =
    str.toList.map(_.toString().toInt)
}
