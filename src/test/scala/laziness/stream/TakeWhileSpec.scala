package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class TakeWhileSpec extends FlatSpec {
  val s = Stream(1, 2, 3, 4)

  "test takeWhile function implemented foldRight" should "success" in {
    val r = s.takeWhileFoldRight(i => i < 3)
    assert(r.toListFaster == List(1, 2))
  }
}
