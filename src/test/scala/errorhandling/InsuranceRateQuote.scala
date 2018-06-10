package test.fpscala.errorhandling

import org.scalatest._
import fpscala.errorhandling._
import scala.math._

class InsuranceRateQuoteSpec extends FlatSpec {
  // def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  //   a flatMap(aa => b map(bb => f(aa, bb)))
  // }

  def map2[A, B, C](a:Option[A], b:Option[B])(f: (A, B) => C): Option[C] = {
    for{
      aa <- a
      bb <- b
    } yield {
      f(aa, bb)
    }
  }


  def map3[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f:(A, B, C) => D): Option[D] = a flatMap(aa => b flatMap (bb => c map (cc => f(aa, bb, cc))))

  def map3_2[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f:(A, B, C) => D): Option[D] = {
    for {aa <- a
      bb <- b
      cc <- c
    } yield {f(aa,bb,cc)}
  }
  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {case e: Exception => None}
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val opAge = Try { age.toInt }
    val opTickets = Try { numberOfSpeedingTickets.toInt }
    map2(opAge, opTickets)(insuranceRateQuote)
  }

  def insuranceRateQuote(age: Int, tickes: Int):Double = age * tickes


  "test insuranceRateQuote function" should "success" in {
    val r = parseInsuranceRateQuote("3","4")
    println(r)
    assert(true)
  }

}
