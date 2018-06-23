package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class FromSpec extends FlatSpec {
  "test infinite function from" should "success" in {
    val r = Stream.from(5)
    println(r.take(6).toList)
    assert(true)
  }
}
