package test.fpscala.datastructures

import org.scalatest._

import fpscala.datastructures._

class FoldLeftAndRightInteroperateSpec extends FlatSpec {
  "Use original foldRight function calculate sum" should "same as foldRightViaFoldLeft function" in {
    val a = List(1, 2, 3)
    val s1 = List.foldRigt(a, 0)(_ + _)
    val s2 = List.foldRigtViaFoldLeft(a, 0)(_ + _)
    assert(s1 == s2)
 }
}
