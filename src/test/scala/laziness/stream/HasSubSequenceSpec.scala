package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class HasSubSequenceSpec extends FlatSpec {

  val s = Stream(1, 2, 3, 4)
  val s1 = Stream(3, 4)
  assert(s.hasSubSequence(s1))
}
