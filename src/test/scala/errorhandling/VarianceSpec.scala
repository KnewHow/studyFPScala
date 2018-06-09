package test.fpscala.errorhandling

import org.scalatest._
import fpscala.errorhandling._
import scala.collection.immutable.Seq
import scala.math._

class VarianceSpec extends FlatSpec {
  def variance(xs:Seq[Double]) :Option[Double] = {
    def cal:Double = {
      val ava = xs.sum / xs.size
      (xs.map(x => pow(x-ava ,2)).sum) / (xs.size)
    }

    xs.size match {
      case 0 => fpscala.errorhandling.None
      case _ => fpscala.errorhandling.Some(cal)
    }
  }
}
