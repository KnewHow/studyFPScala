package fpscala.parsing

sealed trait Parser[PaserError, Parser[+ _]] {
  self =>
  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]
}

trait PaserError
