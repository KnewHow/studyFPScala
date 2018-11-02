package fpscala.monoid

import prop.gen._
import fpscala.basic.Logger.Logger

trait Monoid[A] {
  def op(a: A, b: A): A
  def zero: A
  def law(gen: Gen[A]): Prop = {
    Prop.forAll(
      for {
        x <- gen
        y <- gen
        z <- gen
      } yield (x, y, z)
    ) { g =>
      // Logger.info(s"g->$g")
      op(g._1, op(g._2, g._3)) == op(op(g._1, g._2), g._3) &&
      op(g._1, zero) == op(zero, g._1)
    }
  }
}

case object StringMoniod extends Monoid[String] {
  def op(a: String, b: String): String = a + b
  def zero                             = ""
}

case class ListMonoid[A]() extends Monoid[List[A]] {
  def op(a: List[A], b: List[A]): List[A] = a ++ b
  def zero                                = Nil
}

case object IntAddation extends Monoid[Int] {
  def op(a: Int, b: Int): Int = a + b
  def zero                    = 0
}

case object IntMultiplication extends Monoid[Int] {
  def op(a: Int, b: Int): Int = a * b
  def zero                    = 1
}

case object BooleanOr extends Monoid[Boolean] {
  def op(a: Boolean, b: Boolean) = a || b
  def zero                       = false
}

case object BooleanAnd extends Monoid[Boolean] {
  def op(a: Boolean, b: Boolean): Boolean = a && b
  def zero                                = true
}

case class OptionMonoid[A]() extends Monoid[Option[A]] {
  def op(a: Option[A], b: Option[A]): Option[A] = a orElse b
  def zero                                      = None
}

case class EndMonoid[A]() extends Monoid[A => A] {
  def op(a: (A => A), b: (A => A)): A => A = a andThen b
  def zero                                 = (a: A) => a
}
