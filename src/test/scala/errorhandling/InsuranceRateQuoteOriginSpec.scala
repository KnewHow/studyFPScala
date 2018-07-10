package test.fpscala.errorhandling
import org.scalatest._

class InsuranceRateQuoteOriginSpec extends FlatSpec {

  def parseInsuranceRateQuote(
      age: String,
      numberOfSpeedingTickets: String): Option[Double] = {
    for {
      a <- Option(age.toInt)
      b <- Option(numberOfSpeedingTickets.toInt)
    } yield insuranceRateQuote(a, b)

  }

  def insuranceRateQuote(age: Int, tickes: Int): Double = age * tickes

  "test insuranceRateQuote function" should "success" in {
    val times = 100000000L
    val n = 999
    val m = 999
    for (i <- 1L to times) {
      val re = parseInsuranceRateQuote(n.toString, m.toString)
      assert(re == Some(n * m * 1.0))
    }
  }
}
