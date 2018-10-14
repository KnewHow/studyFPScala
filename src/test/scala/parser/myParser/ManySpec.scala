package test.fpscala.parsing

import fpscala.basic.Logger.Logger
import org.scalatest._
import fpscala.parsing.ParserImpl.MyParser._
import fpscala.parsing.ParserImpl.MyParser
import fpscala.parsing._

class ManySpec extends FlatSpec {

  def runWithRecuriseiveMany[A](
    input: String,
    p: MyParser[A]): Either[ParseError, A] =
    MyParser.run(p)(input + "x")

  "test many parser with big data with super method" should "stack over flow" in {
    val input = List.fill(10000)("s").foldLeft("")(_ + _)
    val p1    = MyParser.defectiveMany("s")
    val p2    = many("s")
    runWithRecuriseiveMany(input, p2) match {
      case Right(r) =>
        assert(r.foldLeft("")(_ + _) == input)
      case Left(e) =>
        Logger.info(s"test many with big data input fail-> $e")
        succeed
    }
  }
}
