package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class ConstantSpec extends FlatSpec {
  "test infinite stream function contant" should "success" in {
    val s = Stream.constant("1")
    println(s.take(10).toList)
    assert(true)
  }
}
