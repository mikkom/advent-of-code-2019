package aoc

import scala.io.Source

object OpCode {
  val ADD = 1
  val MUL = 2
  val HALT = 99
}

object IntCodes {
  def executeOps(mem: Array[Int], i: Int = 0): Option[Int] = {
    def execBinaryOp(fn: (Int, Int) => Int) = {
      mem(mem(i + 3)) = fn(mem(mem(i + 1)), mem(mem(i + 2)))
      executeOps(mem, i + 4)
    }

    mem(i) match {
      case OpCode.ADD  => execBinaryOp(_ + _)
      case OpCode.MUL  => execBinaryOp(_ * _)
      case OpCode.HALT => Some(mem(0))
      case _           => None
    }
  }

  def execProgram(program: List[Int], noun: Int, verb: Int) = {
    val memory = program.toArray
    memory(1) = noun
    memory(2) = verb
    executeOps(memory)
  }

  def answerPart1(program: List[Int]) =
    execProgram(program, noun = 12, verb = 2)

  def answerPart2(program: List[Int], desiredOutput: Int) = {
    case class Result(noun: Int, verb: Int, output: Option[Int])

    val INPUT_LIMIT = 100
    val evaluations = for {
      x <- 0 until INPUT_LIMIT
      y <- 0 until INPUT_LIMIT
    } yield Result(x, y, execProgram(program, x, y))

    evaluations
      .filter(_.output == Some(desiredOutput))
      .headOption
      .map(res => INPUT_LIMIT * res.noun + res.verb)
  }
}

object Day2 extends App {
  import IntCodes._

  val input = Source.fromResource("input-day2.txt").getLines().toList.head
  val program = input.split(',').map(_.toInt).toList

  println("Day 2 pt 1: " + answerPart1(program).getOrElse("N/A"))
  println("Day 2 pt 2: " + answerPart2(program, 19690720).getOrElse("N/A"))
}
