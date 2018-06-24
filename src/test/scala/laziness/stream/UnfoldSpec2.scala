package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class UnfoldSpec2 extends StreamBaseSpec{
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


}
