package fpscala.parsing

/**
  * Parsing JSON string into JSON objects defined following
  * Solution:
  *  As Object JSON, for example: {}, first we should get content exclusive {}, we can use regex to complete.
  *  Then we can read by comma and convert them to JSON object in Scala
  *
  *  As a JSON array, we also recognise by [], then deal each with JSON object.
  *
  */
trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err, Parser[+ _]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._
    // def object: Parser[JObject] = ???
    // def array[JArray] = ???
    succeed(JNumber(12))
  }
}
