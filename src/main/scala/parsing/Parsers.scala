package fpscala.parsing

sealed trait Parser[PaserError, Parser[+ _]] {
  self =>
  def run[A](p: Parser[A])(input: String): Either[PaserError, A]
  def char[A](c: Char): Parser[Char]
  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]
  def map[A, B](f: A => B): Parser[B]
  def flatMap[A, B](f: A => Parser[B]): Parser[B]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }
}

trait PaserError
