package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class ForAllSpec extends StreamBaseSpec {

  "test forAll function implemented with foldRight" should "success" in {
    val r = s.forAll(i => i < 5)
    assert(r)
  }
}
