package aoc

import cats.Monoid
import magnolia.{CaseClass, Magnolia}

object MonoidDerivation {
  type Typeclass[T] = Monoid[T]

  def combine[T](ctx: CaseClass[Monoid, T]): Monoid[T] = new Monoid[T] {
    override def empty: T = ctx.construct(p => p.typeclass.empty)
    override def combine(x: T, y: T): T =
      ctx.construct(p => p.typeclass.combine(p.dereference(x), p.dereference(y)))
  }

  def gen[T]: Monoid[T] = macro Magnolia.gen[T]
}

