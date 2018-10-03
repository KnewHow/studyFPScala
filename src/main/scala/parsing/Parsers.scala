package fpscala.parsing

import fpscala.testing.Prop._
import fpscala.testing._

sealed trait Parser[PaserError, Parser[+ _]] {
  self =>
  def run[A](p: Parser[A])(input: String): Either[PaserError, A]
  def char[A](c: Char): Parser[Char]
  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]
  def map[A, B](p: Parser[A])(f: A => B): Parser[B]
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  def many[A](p: Parser[A]): Parser[List[A]]
  def charViaString(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)
  implicit def asStringParser[A](a: A)(
      implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  /**
    * validate Laws
    */
  object Laws {
    def equals[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in) { s =>
        run(p1)(s) == run(p2)(s)
      }

    def equalsWithValue[A](p: Parser[A], a: A)(in: Gen[String]): Prop =
      forAll(in) { s =>
        run(p)(s) == Right(a)
      }

    def mapLaw[A, B](p: Parser[A])(f: A => B)(in: Gen[String]): Prop =
      equals(map(p)(a => a), p)(in)

    def succeedLaw[A](a: A)(in: Gen[String]): Prop =
      equalsWithValue(succeed(a), a)(in)
  }

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)
    def numA: Parser[Int] = many.map(_.size)
  }
}

trait PaserError
