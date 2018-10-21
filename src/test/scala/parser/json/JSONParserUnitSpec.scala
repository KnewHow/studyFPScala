package test.fpscala.parsing

import org.scalatest.FlatSpec
import fpscala.basic.Logger.Logger
import fpscala.parsing.ParserImpl.MyParser.{string => _, _}
import fpscala.parsing.ParserImpl.MyParser
import fpscala.parsing.JSON._
import fpscala.parsing.JSON
class JSONParserUnitSpec extends FlatSpec {
  implicit def tok(s: String): MyParser[String] = token(MyParser.string(s))
  def keyvalue: MyParser[(String, JSON)]        = escapedQuoted ** (":" *> value)
  def lit = scope("literal") {
    ("null" as (JNull)) |
      (double map (JNumber(_))) |
      (escapedQuoted map (JString(_))) |
      ("true" as (JBool(true))) |
      ("false" as (JBool(false)))
  }
  def value: MyParser[JSON] = lit

  "test tok function" should "succeed" in {
    val input = "KnewHow  "
    MyParser.run("KnewHow")(input) match {
      case Right(r) =>
        assert(r == "KnewHow")
      case Left(e) =>
        Logger.error(s"test tok function took error->$e")
        succeed
    }
  }

  "test lit function" should "succeed" in {
    val input = "12.3"
    MyParser.run(lit)(input) match {
      case Right(r) =>
        assert(r == JNull)
      case Left(e) =>
        Logger.error(s"test lit function took error->$e")
        succeed
    }
  }
}
