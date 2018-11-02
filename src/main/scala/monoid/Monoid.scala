package fpscala.monoid

trait Monoid[A] {
  def op(a: A, b: A): A
  def zero: A
}

case object StringMoniod extends Monoid[String] {
  def op(a: String, b: String): String = a + b
  def zero                             = ""
}

case class ListMonoid[A]() extends Monoid[List[A]] {
  def op(a: List[A], b: List[A]): List[A] = a ++ b
  def zero                                = Nil
}

case object IndAddation extends Monoid[Int] {
  def op(a: Int, b: Int): Int = a + b
  def zero                    = 0
}

case object Multiplication extends Monoid[Int] {
  def op(a: Int, b: Int): Int = a * b
  def zero                    = 1
}

case object booleanOr extends Monoid[Boolean] {
  def op(a: Boolean, b: Boolean) = a || b
  def zero                       = false
}

case object booleanAnd extends Monoid[Boolean] {
  def op(a: Boolean, b: Boolean): Boolean = a && b
  def zero                                = true
}

case class optionMonoid[A]() extends Monoid[Option[A]] {
  def op(a: Option[A], b: Option[A]): Option[A] = a orElse b
  def zero                                      = None
}
