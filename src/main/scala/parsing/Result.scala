package fpscala.parsing

sealed trait Result[+A] {
  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e, c) => Failure(f(e), c)
    case _             => this
  }

  def uncommit: Result[A] = this match {
    case Failure(e, true) => Failure(e, false)
    case _                => this
  }

  def addCommit(b: Boolean): Result[A] = this match {
    case Failure(e, c) => Failure(e, c || b)
    case _             => this
  }

  def advanceSuccess(m: Int): Result[A] = this match {
    case Success(a, n) => Success(a, n + m)
    case _             => this
  }

}

case class Success[+A](get: A, charConsumed: Int) extends Result[A]

case class Failure(get: ParseError, isCommitted: Boolean)
    extends Result[Nothing]

/**
 * A location represent input string and current parser parsing index
 * @param input The input string
 * @param offset The offset current parser parsing
 */
case class Location(input: String, offset: Int) {

  def advanceBy(n: Int): Location = copy(offset = offset + n)

  def toError(msg: String): ParseError = ParseError(
    List(this -> msg)
  )

  def consumerString(n: Int): String = this.input.substring(offset, offset + n)

  // get line number by offset
  val line = input.slice(0, offset + 1).count(_ == '\n') + 1

  /**
   * get column number by offset, If the input is a line, the offset is column number,
   * but if the input contains '\n', the column number is total length - last line feed
   */
  val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1        => offset + 1
    case startLine => offset - startLine
  }
}

/**
 * parse error stack, it will pull all error message in a stack and tell you overall.
 */
case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = ((loc, msg) :: stack))

  def label(loc: Location, msg: String): ParseError =
    ParseError(List(loc -> msg))

  def latestLoc: Option[Location] = latest.map(_._1)

  def latest: Option[(Location, String)] = stack.lastOption
}
