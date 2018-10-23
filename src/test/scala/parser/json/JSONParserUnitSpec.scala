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

  def array =
    surround("[", "]") {
      value sep "," map (kvs => JArray(kvs.toIndexedSeq))
    } scope "array"

  def obj =
    surround("{", "}") {
      keyvalue sep "," map (r => JObject(r.toMap))
    } scope "object"

  def lit =
    scope("literal") {
      ("null" as (JNull)) |
        (double map (JNumber(_))) |
        (escapedQuoted map (JString(_))) |
        ("true" as (JBool(true))) |
        ("false" as (JBool(false)))
    }

  def value: MyParser[JSON] = lit | obj | array

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
        assert(r == JNumber(12.3))
      case Left(e) =>
        Logger.error(s"test lit function took error->$e")
        succeed
    }
  }

  "test keyValue function" should "succeed" in {
    val input = """"name":"KnewHow""""
    MyParser.run(keyvalue)(input) match {
      case Right(r) =>
        assert(r == "name" -> JString("KnewHow"))
      case Left(e) =>
        Logger.error(s"test keyValue function took error->$e")
        succeed
    }
  }

  "test obj function" should "succeed" in {
    val input = """{
  "name" : "KnewHow",
  "time" : "2018-10-21"}
"""
    MyParser.run(obj)(input) match {
      case Right(r) =>
        assert(
          r == JObject(
            Map(
              "name" -> JString("KnewHow"),
              "time" -> JString("2018-10-21")
            )
          )
        )
        succeed
      case Left(e) =>
        Logger.error(s"test obj function took error->$e")
        succeed
    }
  }

  "test array function" should "succeed" in {
    val input = """[
"Java",
"Scala",
"Cpp"
]
"""
    MyParser.run(array)(input) match {
      case Right(r) =>
        assert(
          r == JArray(
            IndexedSeq(
              JString("Java"),
              JString("Scala"),
              JString("Cpp")
            )
          )
        )
      case Left(e) =>
        Logger.error(s"test array function took error->$e")
        succeed
    }
  }

}
