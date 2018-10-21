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
  case object JNull                          extends JSON
  case class JNumber(get: Double)            extends JSON
  case class JString(get: String)            extends JSON
  case class JBool(get: Boolean)             extends JSON
  case class JArray(get: IndexedSeq[JSON])   extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    import P.{string => _, _}
    implicit def tok(s: String): Parser[String] = token(P.string(s))
    def array =
      surround("[", "]") {
        value sep "," map (vs => JArray(vs.toIndexedSeq))
      } scope "array"
    def obj =
      surround("{", "}") {
        keyvalue sep "," map (r => JObject(r.toMap))
      } scope "object"
    def keyvalue: Parser[(String, JSON)] = escapedQuoted ** (":" *> value)
    def lit: Parser[JSON] = scope("literal") {
      ("null" as (JNull)) |
        (double map (JNumber(_))) |
        (escapedQuoted map (JString(_))) |
        ("true" as (JBool(true))) |
        ("false" as (JBool(false)))
    }
    def value: Parser[JSON] = lit | obj | array
    root(whitespace *> (array | obj))
  }
}
