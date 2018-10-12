package fpscala.parsing

import scala.util.matching.Regex

object ParserImpl {

  type MyParser[+A] = Location => Result[A]

  object MyParser extends Parsers[MyParser] {

    def run[A](p: MyParser[A])(input: String): Either[ParseError, A] = p(Location(input, 0)) match {
      case Success(a, _) => Right(a)
      case Failure(e, _) => Left(e)
    }

    def or[A](p1: MyParser[A], p2: => MyParser[A]): MyParser[A] =
      loc => p1(loc) match {
        case Failure(e, false) => p2(loc)
        case r => r
      }

    def label[A](msg: String)(p: MyParser[A]): MyParser[A] = loc => p(loc) mapError(_.label(loc, msg))

    def scope[A](msg: String)(p: MyParser[A]): MyParser[A] =
      loc => p(loc) mapError(e => e.push(loc, msg))

    def slice[A](p: MyParser[A]): MyParser[String] =
      loc => p(loc) match {
        case Success(_, n) => Success(loc.consumerString(n), n)
        case f@Failure(_, _) => f
      }

    def attempt[A](p: MyParser[A]): MyParser[A] =
      loc => p(loc).uncommit

    def flatMap[A, B](p: MyParser[A])(f: A => MyParser[B]): MyParser[B] =
      loc => p(loc) match {
        case Success(a, n) =>
          f(a)(loc.advanceBy(n))
            .addCommit(n != 0)
            .advanceSuccess(n)
        case f@Failure(_, _) => f
      }


    /**
     * In this implement, regex matching is all-or-nothing.
     */
    implicit def regex(r: Regex): MyParser[String] =
      loc => r.findPrefixOf(loc.input) match {
        case Some(s) =>
          Success(s, s.length)
        case None =>
          val msg = s"regex $r"
          Failure(loc.toError(msg), false)
      }

    implicit def string(s: String): MyParser[String] =
      loc => {
        val r = firstNonMatchingIndex(loc.input, s, loc.offset)
        r match {
          case -1 => Success(s, s.length)
          case _ =>
            val msg = s"'$s'"
            Failure(loc.advanceBy(r).toError(msg), r != 0)
        }
      }

    def errorLocation(e: ParseError): Location = ???

    def errorMessage(e: ParseError): String = ???
  }

  /**
   * If s1.substring(offset).startWith(s2), return -1,
   * otherwise return the first index where the two string
   * differed. If s2 longer than s1, return s1.length
   */
  private def firstNonMatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while(i < s1.length && i < s2.length) {
      if (s1.charAt(offset + 1) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length - offset >= s2.length) -1 else s1.length - offset
  }

}
