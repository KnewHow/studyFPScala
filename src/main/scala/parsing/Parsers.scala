package fpscala.parsing

import fpscala.testing.Prop._
import fpscala.testing._

sealed trait Parser[PaserError, Parser[+ _]] {
  self =>
  def run[A](p: Parser[A])(input: String): Either[PaserError, A]
  def char[A](c: Char): Parser[Char]
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def map[A, B](p: Parser[A])(f: A => B): Parser[B]
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(
      f: (A, B) => C): Parser[C] =
    map(product(p1, p2))(f.tupled)
  def map2Native[A, B, C](p1: Parser[A], p2: => Parser[B])(
      f: (A, B) => C): Parser[C]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case m if m <= 0 => succeed(List())
    case _           => map2(p, listOfN(n - 1, p))(_ :: _)
  }
  def wrap[A](a: => Parser[A]): Parser[A]
  def slice[A](p: Parser[A]): Parser[String]
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)]
  def productViaMap2[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    map2Native(p1, p2)(_ -> _)
  def charViaString(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)
  implicit def asStringParser[A](a: A)(
      implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def slice[A]: Parser[String] = self.slice(p)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def numA: Parser[Int] = char('a').many.map(_.size)
    def count(a: Char, b: Char): Parser[(Int, Int)] =
      many.slice.map(_.size) ** many1.slice.map(_.size)
  }

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

    /**
      * product Law is similar with multiplication associative, a * (b * c) ==  (a * b) * c,
      * In Parse, The only diff is: a ** (b ** c) == (a ** b) ** c
      */
    def productLaw[A, B, C](p1: Parser[A], p2: Parser[B], p3: Parser[C])(
        in: Gen[String]): Prop =
      equals(
        (p1 ** p2) ** p3 map (unbiasL),
        p1 ** (p2 ** p3) map (unbiasR)
      )(in)

    /**
      * convert left nested to 3-tuples
      */
    private def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) =
      (p._1._1, p._1._2, p._2)

    /**
      * convert right nested to 3-tuple
      */
    private def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) =
      (p._1, p._2._1, p._2._2)
  }

}

trait PaserError
