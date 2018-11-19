package test.fpscala.state

import org.scalatest._
import fpscala.state._
import fpscala.basic.Logger.Logger

class StateSpec extends FlatSpec {
  val rng = RNGViaState(42)
  "test next int function" should "success" in {
    val s1 = rng.nextInt
    val r1 = s1.run(rng)
    val r2 = s1.run(r1._2)
    assert(true)
  }

  "test nonNegative function" should "success" in {
    val s1 = rng.nonNegativeInt
    val r1 = s1.run(rng)
    val r2 = s1.run(r1._2)
    assert(r1._1 >= 0 && r2._1 >= 0)
  }

  "test nonNegativeEvent function" should "success" in {
    val s1 = rng.nonNegativeEvent
    val r1 = s1.run(rng)
    val r2 = s1.run(r1._2)
    assert(r1._1 % 2 == 0 && r2._1 % 2 == 0)
  }

  "test nonEvents function" should "success" in {
    val events = rng.nonEvents(10)
    val r = events.run(rng)._1.forall { i =>
      Logger.info(s"i-> $i")
      i % 2 == 0
    }
    assert(r)
  }

  "test state get function" should "success" in {
    val s = rng.nonEvents(10)
    val r = State.get
    assert(true)
  }
}

case class RNGViaState(seed: Long) {
  type Rand[A] = State[RNGViaState, A]

  def nextInt: Rand[Int] =
    new Rand[Int](
      r => {
        val newSeed = (r.seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = RNGViaState(newSeed)
        val n       = (newSeed >>> 16).toInt
        n -> nextRNG
      }
    )

  def nonNegativeInt: Rand[Int] = nextInt.flatMap { i =>
    new Rand[Int](
      r => {
        if (i < 0) {
          (-(i + 1)) -> r
        } else {
          i -> r
        }
      }
    )
  }

  def nonNegativeEvent: Rand[Int] = nextInt.map { i =>
    i - i % 2
  }

  def nonEvents(n: Int): Rand[List[Int]] = State.sequence(
    List.fill(n)(nonNegativeEvent)
  )

}
