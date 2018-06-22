package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class ExsitSpec extends FlatSpec {
  val s = Stream(1, 2, 3, 4)

  "test original exists function" should "success" in {
    val r = s.originalExists(i => i % 2 == 0)
    assert(r)
  }

  "test foldRightExists function" should "success" in {
    val r = s.foldRightExists(i => i % 2 == 0)
    assert(r)
  }
}
