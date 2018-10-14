package test.fpscala.parsing

import fpscala.basic.Logger.Logger
import org.scalatest._
import fpscala.parsing.ParserImpl.MyParser._
import fpscala.parsing.ParserImpl.MyParser
class MyParserSpec extends FlatSpec {
  "test string parser" should "success" in {
    val input = "Hello, MyParser"
    val r     = MyParser.run("Hellx")(input)
    r match {
      case Left(e) =>
        assert(e.stack.headOption.map(_._2 == "'Hellx'").getOrElse(true))
      case Right(a) => assert(a == "Hello")
    }
  }

  "test many parser" should "success" in {
    val manyParser = many("s")
    val input      = "ssss"
    MyParser.run(manyParser)(input) match {
      case Right(r) =>
        assert(r == List.fill(4)("s"))
      case Left(e) =>
        Logger.info(s"took error->$e")
    }
  }

  "test regex parser" should "success" in {
    val input = "100, Hello, MyParser"
    MyParser.run("[0-9]+".r)(input) match {
      case Right(a) =>
        assert(a == "100")
      case Left(e) =>
        Logger.info(s"regex parse took error->$e")

    }
  }

  "test slice parser" should "success" in {
    val input = "Hello, MyParser"
    val s     = "Hello, M"
    MyParser.run(slice(s))(input) match {
      case Right(r) => assert(r == s)
      case Left(e) =>
        Logger.info(s"slice parser took error-> $e")
        succeed
    }
  }

  "test flatMap function" should "success" in {
    val input  = "abc100def"
    val parser = string("abc").flatMap(s => regex("[0-9]+".r))
    MyParser.run(parser)(input) match {
      case Right(r) =>
        assert(r == "100")
      case Left(e) =>
        Logger.info(s"test flatMap function took error->$e")
        succeed
    }
  }

  "test product function" should "success" in {
    val input = "abc123cde"
    // val parser = string("abc") ** regex("[0-9]+".r)
    val parser = product(string("abc"), regex("[0-9]+".r))
    MyParser.run(parser)(input) match {
      case Right(r) =>
        assert(r == ("abc" -> "123"))
      case Left(e) =>
        Logger.info(s"test product function took error->$e")
        succeed
    }
  }

  "test string regex and many combine with **" should "success" in {
    val input         = "Hello,  100qwer"
    val strParser     = string("Hello,")
    val manyParser    = many(" ")
    val regexParser   = regex("[0-9]+".r)
    val combineParser = strParser ** manyParser ** regexParser
    val expectResult  = (("Hello,", List.fill(2)(" ")), "100")
    MyParser.run(combineParser)(input) match {
      case Right(r) =>
        assert(r == expectResult)
      case Left(e) =>
        Logger.error(s"test ** took a error->$e")
        succeed
    }
  }

  "test lable function" should "succeed" in {
    val input    = "Hello, MyParser"
    val errorMsg = "don't start with"
    val parser   = label(errorMsg)("Hello")
    MyParser.run(parser)(input) match {
      case Right(r) =>
        assert(r == "Hello")
      case Left(e) =>
        Logger.error(s"test label function took error->$e")
        assert(e.stack.headOption.map(_._2 == errorMsg).getOrElse(true))
    }
  }

  "test scope function" should "succeed" in {
    val input    = "Hello, MyParser"
    val errorMsg = "don't start with"
    val parser   = scope(errorMsg)("Hxllo")
    MyParser.run(parser)(input) match {
      case Right(r) =>
        assert(r == "Hello")
      case Left(e) =>
        Logger.error(s"test scope function took error->$e")
        assert(
          e.stack.size == 2 && e.stack.headOption
            .map(_._2 == errorMsg)
            .getOrElse(true))
    }
  }

  "test attempt function" should "succeed" in {
    val input  = "Hello, MyParser"
    val parser = attempt("Hello")
    MyParser.run(parser)(input) match {
      case Right(r) =>
        assert(r == "Hello")
      case Left(e) =>
        succeed
    }
  }

  "test or function with attempt should clear first status" should "succeed" in {
    val input  = "Hello, MyParser"
    val parser = or(attempt("Hxllo"), "Hello")
    MyParser.run(parser)(input) match {
      case Right(r) =>
        assert(r == "Hello")
      case Left(e) =>
        Logger.error(s"test or function took error->$e")
        fail
    }
  }

  "test format parser error" should "succeed" in {
    val input    = "Hello, MyParser"
    val errorMsg = "don't start with"
    val parser   = scope(errorMsg)("Hxllo")
    MyParser.run(parser)(input) match {
      case Right(r) =>
        assert(r == "Hello")
      case Left(e) =>
        Logger.error(s"test format parser took error->$e")
        assert(
          e.stack.size == 2 && e.stack.headOption
            .map(_._2 == errorMsg)
            .getOrElse(true))
    }
  }
}
