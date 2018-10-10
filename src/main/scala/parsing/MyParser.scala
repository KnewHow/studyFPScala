package fpscala.parsing

import scala.util.matching.Regex

object ParserImpl {

  sealed trait Result[+A]

  case class Success[+A](get: A, charConsumed: Int) extends Result[A]

  case class Failure(get: ParseError) extends Result[Nothing]

  type MyParser[+A] = Location => Result[A]


  object MyParser extends Parsers[MyParser] {

    def run[A](p: MyParser[A])(input: String): Either[ParseError, A] = ???

    def or[A](p1: MyParser[A], p2: => MyParser[A]): MyParser[A] = ???

    def label[A](msg: String)(p: MyParser[A]): MyParser[A] = ???

    def scope[A](msg: String)(p: MyParser[A]): MyParser[A] = ???

    def slice[A](p: MyParser[A]): MyParser[String] = ???

    def attempt[A](p: MyParser[A]): MyParser[A] = ???

    def flatMap[A, B](p: MyParser[A])(f: A => MyParser[B]): MyParser[B] = ???

    implicit def regex(r: Regex): MyParser[String] = ???

    implicit def string(s: String): MyParser[String] = (l: Location) => {
      if (l.input.startsWith(s))
        Success(l.input, s.length)
      else
        Failure(ParseError(List(l -> s)))
    }

    def errorLocation(e: ParseError): Location = ???

    def errorMessage(e: ParseError): String = ???
  }

}
