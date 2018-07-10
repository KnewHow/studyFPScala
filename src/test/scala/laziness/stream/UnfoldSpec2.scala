package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class UnfoldSpec2 extends StreamBaseSpec {
  val s1 = Stream(1, 2, 3, 4)
  val s2 = Stream(4, 5, 6)

  "implement map function with unfold myself" should "success" in {
    val r = s.mapViaUnFold(i => i.toString)
    assert(r.toList == List("1", "2", "3", "4"))
  }

  "implement map function with unfold standard key and complete function body" should "success" in {
    val r = s.mapViaUnFold2(i => i.toString)
    assert(r.toList == List("1", "2", "3", "4"))
  }

  "implement map function with unfold standard key and function body abbreviation" should "success" in {
    val r = s.mapViaUnFold3(i => i.toString)
    assert(r.toList == List("1", "2", "3", "4"))
  }

  "implement take function with unfold funciton" should "success" in {
    val r = s.takeViaUnFold(2)
    assert(r.toList == List(1, 2))
  }

  "implement takeWhile function with unfold function" should "success" in {
    val r = s.takeWhileViaUnfold(i => i < 4)
    assert(r.toList == List(1, 2, 3))
  }

  "implent zipWith function with unfold function" should "success" in {
    val r = s1.zipWith(s2)(_ + _)
    assert(r.toList == List(5, 7, 9))
  }

  "implent zip function with zipAll" should "success" in {
    val r = s1.zip(s2)
    assert(
      r.toList == List(
        1 -> 4,
        2 -> 5,
        3 -> 6
      ))
  }

  "implement zipAll function with unfold function" should "success" in {

    val r = s1.zipAll(s2)
    assert(
      r.toList == List(
        (Some(1), Some(4)),
        (Some(2), Some(5)),
        (Some(3), Some(6)),
        (Some(4), None)
      ))
    assert(true)
  }

}
