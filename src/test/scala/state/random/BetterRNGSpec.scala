package test.fpscala.state

import org.scalatest._
import fpscala.state._

class BetterRNGSpec extends FlatSpec {
  val rng = SimpleRNG(42)

  "use state action to implent nextInt function" should "success" in {
    val r = rng.int(rng)
    println(r)
    println(rng.int(r._2))
    assert(true)
  }

  "test rand map function" should "success" in {
    val r = rng.int
    val r1 = rng.map(r)(i => i / 10.0)
    println(s"r -> $r")
    println(s"r1 -> ${r1}")
    println(s"r1 call -> ${r1(rng)}")
    assert(true)
  }

  "implement nonNegativeIntEvent function" should "success" in {
    val r = rng.nonNegativeIntEvent
    assert(r(rng)._1 % 2 == 0)
  }

  "implement double with map function" should "success" in {
    val r = rng.doubleViaMap
    println(r(rng))
    assert(true)
  }

  "implement map2 function" should "success" in {
    val r = rng.map2(rng.int, rng.int)((a, b) => -a + b)
    println(s"r -> ${r(rng)}")
    assert(true)
  }

  "implemt both function with map2 function" should "success" in {
    val r = rng.both(rng.int, rng.double)
    println(s"both -> ${r(rng)}")
    assert(true)
  }

  "implement intDoule with both function" should "success" in {
    val r = rng.intDoubleViaBoth
    assert(r(rng)._1 == rng.intDouble(rng)._1)
  }

  "implement sequence function with foldright and map2" should "success" in {
    val fs = List.fill(10)(rng.int)
    val r = rng.sequence(fs)
    println(s"sequence -> ${r(rng)}")
    assert(true)
  }

  "implement ints with with sequence function" should "success" in {
    val r = rng.intsViaSequence(10)
    val r1 = r(rng)._1
    val r2 = rng.ints(10)(rng)._1
    assert(r1 == r2)
  }

  "test nonNegativeLessThan function" should "success" in {
    val n = 10
    val r1 = rng.nonNegativeLessThan(n)(rng)

    val r2 = rng.nonNegativeLessThan(n)(r1._2)

    val r3 = rng.nonNegativeLessThan(n)(r2._2)

    assert(r1._1 <= 10 && r2._1 <= 10 && r3._1 <= 10)
  }

  "test nonNegativeLessThanViaFlatMap function" should "success" in {
    val n = 10
    val r1 = rng.nonNegativeLessThanViaFlatMap(n)(rng)

    val r2 = rng.nonNegativeLessThanViaFlatMap(n)(r1._2)

    val r3 = rng.nonNegativeLessThanViaFlatMap(n)(r2._2)

    assert(r1._1 <= 10 && r2._1 <= 10 && r3._1 <= 10)
  }

  "implement map function with flatMap" should "success" in {
    def nonNegativeIntEvent =
      rng.mapViaFlatMap(rng.nonNegativeInt)(i => i - i % 2)

    val r1 = nonNegativeIntEvent(rng)
    assert(r1._1 % 2 == 0)
  }

  "implement map2 function with flatMap" should "success" in {
    val as = List.fill(3)(rng.unit(8))
    val r = rng.sequence2(as)(rng)
    assert(r._1 == List(8, 8, 8))
  }
}
