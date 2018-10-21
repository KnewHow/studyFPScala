package test.fpscala.parsing

import fpscala.basic.Logger.Logger
import org.scalatest.FlatSpec
import fpscala.parsing.JSON
import fpscala.parsing.ParserImpl.MyParser

class JSONParserSpec extends FlatSpec {
  val jsonTxt = """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""

  val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
"""

  val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
"""

  def printResult[E](e: Either[E, JSON]) =
    e.fold(println, println)

  val json: MyParser[JSON] = JSON.jsonParser(MyParser)

  "test parse json" should "success" in {
    printResult { MyParser.run(json)(malformedJson2) }
    succeed
  }
}
