package test.fpscala.testing

import org.scalatest._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class ScalaTestSpec
    extends FlatSpec
    with PropertyChecks
    with Matchers
    with GeneratorDrivenPropertyChecks {
  "test ScalaTest forAll function" should "success" in {
    forAll { (a: Int, b: Int) =>
      println(s"a->$a,b->$b")
      assert(a + b == (a + b))
    }
  }

  "All Fraction" should "statisfy following commnad" in {
    val validNumers =
      for (n <- Gen.choose(Integer.MIN_VALUE + 1, Integer.MAX_VALUE)) yield n
    val validDenoms =
      for (d <- validNumers if d != 0) yield d
    forAll(
      (validNumers, "n"),
      (validDenoms, "d"),
      minSuccessful(500),
      maxDiscarded(300)) { (n: Int, d: Int) =>
      whenever(
        d != 0 && d != Integer.MIN_VALUE
          && n != Integer.MIN_VALUE) {

        val f = new Fraction(n, d)

        if (n < 0 && d < 0 || n > 0 && d > 0)
          f.numer should be > 0
        else if (n != 0)
          f.numer should be < 0
        else
          f.numer should be === 0

        f.denom should be > 0
      }
    }
  }

  class Fraction(n: Int, d: Int) {

    require(d != 0)
    require(d != Integer.MIN_VALUE)
    require(n != Integer.MIN_VALUE)

    val numer = if (d < 0) -1 * n else n
    val denom = d.abs

    override def toString = numer + " / " + denom
  }

}
