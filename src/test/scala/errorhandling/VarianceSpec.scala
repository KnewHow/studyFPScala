package test.fpscala.errorhandling

import org.scalatest._
import fpscala.errorhandling._
import scala.collection.immutable.Seq
import scala.math._

class VarianceSpec extends FlatSpec {
  def mean(xs: Seq[Double]): Option[Double] = {
    if(xs.isEmpty) fpscala.errorhandling.None
    else fpscala.errorhandling.Some(xs.sum / xs.size)
  }
  def variance(xs:Seq[Double]) :Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(e => pow(e-m, 2))))
  }

  "use flatMap implement variance function" should "success" in {
    val seq = Seq(1.0 ,2.0 ,3.0)
    val r = variance(seq)
    println(r)
    assert(true)
  }
}
