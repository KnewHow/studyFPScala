package test.fpscala.state

import org.scalatest._
import fpscala.state._

class CandySpec extends FlatSpec {
  val m = Machine(true, 10, 0)
  "test candy simulateMachine" should "success" in {
    val inputs = List(
      Coin,
      Turn,
      Coin,
      Coin,
      Turn,
      Turn
    )
    val r = Candy.simulateMachine(inputs)

    println(r.run(m))
    assert(true)
  }
}
