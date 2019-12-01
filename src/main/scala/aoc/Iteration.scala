package aoc

import cats.syntax.option._

object Iteration {
  private def tupled[A](a: A): (A, A) = (a, a)

  def iterate[A](init: A)(f: A => A): LazyList[A] =
    LazyList.unfold(init)(x => tupled(f(x)).some)
}
