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

}
