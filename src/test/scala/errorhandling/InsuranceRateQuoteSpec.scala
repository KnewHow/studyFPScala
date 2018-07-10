package test.fpscala.errorhandling

import org.scalatest._
import fpscala.errorhandling._
import scala.math._
import scala.util._

class InsuranceRateQuoteSpec extends FlatSpec {
  // def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  //   a flatMap(aa => b map(bb => f(aa, bb)))
  // }

  val r = new Random(1000)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    } yield {
      f(aa, bb)
    }
  }

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch { case e: Exception => None }
  }

  def strNumberMuti_2(age: String,
                      numberOfSpeedingTickets: String): Option[Double] = {
    val opAge = Try { age.toInt }
    val opTickets = Try { numberOfSpeedingTickets.toInt }
    map2(opAge, opTickets)(_ * _)
  }

  def strNumberMuti(s1: String, s2: String): Option[Double] = {
    for {
      a <- Try { s1.toInt }
      b <- Try { s2.toInt }
    } yield a * b

  }

  def insuranceRateQuote(age: Int, tickes: Int): Double = age * tickes

  "test insuranceRateQuote function" should "success" in {
    // forAll{ (m: Int, n: Int) =>
    //   val r = parseInsuranceRateQuote_2(m.toString, n.toString)
    //   println(r)
    //   assert(r == Some(m * n * 1.0))
    // }
    val times = 10000000L
    val n = 999
    val m = 999
    for (i <- 1L to times) {
      val re = strNumberMuti(n.toString, m.toString)
      assert(re == Some(n * m * 1.0))
    }
  }

}
