package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class FindSpec extends FlatSpec {
  val s = Stream(1, 2, 3)
  "test find function" should "success" in {
    val s1 = s.find(_ < 3)
    val s2 = s.find(_ > 3)
    assert(s1 == Some(1))
    assert(s2 == None)
  }
}
