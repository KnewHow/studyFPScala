package test.fpscala.errorhandling.either

import org.scalatest._
import fpscala.errorhandling._

class InsuranceRateQuoteSpec extends FlatSpec {
  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String):Either[Exception, Double] = {
    for{
      a <- Either.Try(age.toInt)
      b <- Either.Try(numberOfSpeedingTickets.toInt)
    } yield insuranceRateQuote(a, b)
  }

  def insuranceRateQuote(age: Int, numTickets: Int): Double = age * numTickets

  "use either type implement insuranceRateQuote" should "success" in {
    val r = parseInsuranceRateQuote("3ds","4")
    println(r)
    assert(true)
  }
}
