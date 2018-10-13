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

  // "test many parser with big data with super method" should "stack over flow" in {
  //   val input = List.fill(100)("s").foldLeft("")(_ + _)
  //   MyParser.run(MyParser.defectiveMany("s"))(input + "x") match {
  //     case Right(r) =>
  //       assert(r.foldLeft("")(_ + _) == input)
  //     case Left(e) =>
  //       Logger.info(s"test many with big data input fail-> $e")
  //       succeed
  //   }
  // }

  "test many parser with big data with prefect method" should "stack over flow" in {
    val input = List.fill(10000)("s").foldLeft("")(_ + _)
    MyParser.run(MyParser.many("s"))(input + "x") match {
      case Right(r) =>
        assert(r.foldLeft("")(_ + _) == input)
      case Left(e) =>
        Logger.info(s"test many with big data input fail-> $e")
        succeed
    }
  }

  "test flatMap function" should "success" in {
    val input  = "100abcdef"
    val parser = string("100ab").flatMap(s => regex("[0-9]+".r))
    MyParser.run(parser)(input) match {
      case Right(r) =>
        assert(r == "100")
      case Left(e) =>
        Logger.info(s"test flatMap function took error->$e")
        succeed
    }
  }

  // "test product function" should "success" in {
  //   val input = "123cde"
  //   // val parser = string("12") ** regex("[0-9]+".r)
  //   val parser = product(string("12"), regex("[0-9]+".r))
  //   MyParser.run(parser)(input) match {
  //     case Right(r) =>
  //       assert(r == ("ab" -> "3"))
  //     case Left(e) =>
  //       Logger.info(s"test product function took error->$e")
  //       succeed
  //   }
  // }

  // "test string regex and many combine with **" should "success" in {
  //   val input         = "Hello  ,100qwer"
  //   val strParser     = string("Hello")
  //   val manyParser    = many(" ")
  //   val regexParser   = regex("[0-9]".r)
  //   val combineParser = strParser ** manyParser ** regexParser
  //   val expectResult  = (("Hello", List.fill(2)(" ")), "100")
  //   MyParser.run(combineParser)(input) match {
  //     case Right(r) =>
  //       Logger.debug(s"success result->$r")
  //       assert(r == expectResult)
  //     case Left(e) =>
  //       Logger.debug(s"test ** took a error->$e")
  //   }
  // }
}