package fpscala.parsing

import fpscala.testing.Prop._
import java.util.regex.Pattern
import fpscala.testing._
import scala.util.matching.Regex
import fpscala.basic.Logger.Logger

trait Parsers[Parser[+ _]] {
  self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // Primitive methods, which can be used to generate other mehtods

  /**
   * Do | with two parse, the second parameter is lazy evaluaction!
   * If run first parameter failure, it will return a failure result, the second parameter will not be evaluated.
   * If run first parameter successful, then the second parameter will be evaluated then run it.
   */
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  /**
   * If most function, if the parser occur error, we don't know what error message will be tell.
   * But in this function, we can convert a parser to another parser with error message. If you run it and
   * occur error, it will tell the message you assigned.
   * You can get more at `lableLaw`
   * @param msg The error message you can assigned, it will be told when it parser fail
   * @param p The original parser you can convert
   */
  def label[A](msg: String)(p: Parser[A]): Parser[A]

  /**
   * In label function, if you run label parser failure, it will tell a error you assigned,
   * but some times, we want error messages with a called stack. for example, if you run(p)(s) gives you
   * a Left(e1), and you run run(scope(msg)(p))(s) gives you a Left(e2), then e2.stack.head is msg, e2.stack.tail
   * is e1. In a word, scope will add error into a stack, then tell you overall.
   * @param msg The error message you can assigned, it don't override original error message, it will be pushed into
   * the stack after original error message
   * @param p The original parser you can convert
   * @return a new parser with stack error messages
   */
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  /**
   * Conver a Parser[A] to Parser[String], when you run it,
   * it will return the string the parser has scanned when the parser failure
   *  for Example: run("a")("aab") will return "aa"
   */
  def slice[A](p: Parser[A]): Parser[String]

  /**
   *  TODO add comment
   */
  def attempt[A](p: Parser[A]): Parser[A]

  /**
   * Run first Parser, then use it result to generate next Parser
   */
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  /**
   * Convert A to Parser[A]
   *
   */
  def succeed[A](a: A): Parser[A]

  /**
   * parsing regex to Parser[String], when you run it, it will run a string
   * which input pass the regex
   */
  implicit def regex(r: Regex): Parser[String]

  /**
   * convert string to Parser[String], you can read more in stringLaw, When you run it, input string
   * must start with s, otherwise a error will be return
   * @param s The input string must start with it
   */
  implicit def string(s: String): Parser[String]

  def errorMessage(e: ParseError): String =
    e.stack.headOption.map(_._2).getOrElse("")

  // Following methods are not primitive methods, which can be generate by primitive methods
  def map[A, B](p: => Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(r => succeed(f(r)))

  /**
   * Product two parser with (A, B), when you run it, the input string must statify Parser[A] then Parser[B].
   * For example, when you `run(product("ab","cd"))("abcd")` will get ("ab","cd"), but if you run `run(product("ab","cd"))("abed")` will get a error
   * @param p1 The Parser the input must satisfy at first
   * @param p2 The Parser the input must satisfy and then
   */
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p1) { r1 =>
      map(p2) { r2 =>
        r1 -> r2
      }
    }
  def map2Native[A, B, C](p1: Parser[A], p2: => Parser[B])(
    f: (A, B) => C): Parser[C] = flatMap(p1) { r1 =>
    map(p2) { r2 =>
      f(r1, r2)
    }
  }
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(
    f: (A, B) => C): Parser[C] =
    map(product(p1, p2))(f.tupled)

  /**
   * Assigned string whether repeat n times of Parser[A], you can refer `listOfNRaw`
   */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case m if m <= 0 => succeed(List())
    case _           => map2(p, listOfN(n - 1, p))(_ :: _)
  }

  /**
   * Calculating how many times A appear in assigned string.
   * The reuslt is Parser[List[A]] whose size is the times
   * If A don't appear, The list is empty
   */
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  /**
   * Calculating how many times A appear in assigned string.
   * The only diff between many is if the a don't appear, a error will be return
   */
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)
  def productViaMap2[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    map2Native(p1, p2)(_ -> _)
  def char(c: Char): Parser[Char]                       = string(c.toString).map(_.charAt(0))
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)
  implicit def asStringParser[A](a: A)(
    implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  /**
   * Parser which consumes reluctantly until it encounters the given string.
   */
  def thru(s: String): Parser[String] = (".*?" + Pattern.quote(s)).r

  def quoted: Parser[String] = "\"" *> thru("\"").map(_.dropRight(1))

  /**
   * Wrap `p` with start/stop delimiters
   */
  def surround[A](start: Parser[Any], stop: Parser[Any])(
    p: => Parser[A]): Parser[A] =
    start *> p <* stop

  /**
   * attempt `p` and strips tailing whitespace, usually used for token of a grammer
   */
  def token[A](p: Parser[A]): Parser[A] = attempt(p) <* whitespace

  /**
   * match one or more space or \n or \t
   */
  def whitespace: Parser[String] = "\\s*".r

  /**
   * run p and p1, then drop first result
   */
  def skipL[A, B](p1: Parser[A], p2: => Parser[B]): Parser[B] =
    map2(p1, p2)((_, b) => b)

  /**
   * run p and p1, then drop the second result
   */
  def skipR[A, B](p1: Parser[A], p2: => Parser[B]): Parser[A] =
    map2(p1, p2)((a, _) => a)

  case class ParserOps[A](p: Parser[A]) {
    // The function is same with above, it make Parser interact easily
    def |[B >: A](p2: Parser[B]): Parser[B]          = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B]      = self.or(p, p2)
    def many: Parser[List[A]]                        = self.many(p)
    def many1: Parser[List[A]]                       = self.many1(p)
    def map[B](f: A => B): Parser[B]                 = self.map(p)(f)
    def flatMap[B](f: A => Parser[B])                = self.flatMap(p)(f)
    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def slice[A]: Parser[String]                     = self.slice(p)
    def **[B](p2: => Parser[B]): Parser[(A, B)]      = self.product(p, p2)
    def numA: Parser[Int]                            = char('a').many.map(_.size)
    def count(a: Char, b: Char): Parser[(Int, Int)] =
      many.slice.map(_.size) ** many1.slice.map(_.size)

    def <*[B](p1: => Parser[B]): Parser[A] = self.skipR(p, p1)

    def *>[B](p1: Parser[B]): Parser[B] = self.skipL(p, p1)
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

    /**
     * a | b == b | a
     */
    def orLaw[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      equals(or(p1, p2), or(p2, p1))(in)

    def stringLaw(in: Gen[String]): Prop = forAll(in) { s =>
      run(string(s))(s) == Right(s)
    }

    def listOfNRaw(n: Int)(in: Gen[String]): Prop = forAll(in) { s =>
      run(listOfN(n, s))(s * n) == Right(s * n)
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

    def labelLaw[A](msg: String)(p: Parser[A])(in: Gen[String]): Prop =
      forAll(in) { s =>
        run(label(msg)(p))(s) match {
          case Left(e) => errorMessage(e) == msg
          case _       => true
        }
      }

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
