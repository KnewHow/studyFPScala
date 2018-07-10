package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class ScanRightSpec extends FlatSpec {
  val r = Stream(1, 2, 3).scanRight(0)(_ + _)

  assert(r.toList == List(6, 5, 3, 0))
}
