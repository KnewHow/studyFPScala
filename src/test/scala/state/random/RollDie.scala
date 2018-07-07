package test.fpscala.state

import org.scalatest._
import fpscala.state.RNG
import fpscala.state.SimpleRNG

class RollDieSpec extends FlatSpec {
  val rng = SimpleRNG(42)

  "implement rollDia1 function" should "always less or equal six" in {
    val r = rng.rollDie(rng)
    assert(r._1 <= 6)
  }
}
