package test.fpscala.datastructures

import org.scalatest._
import fpscala.datastructures._

class FilterSpec extends FlatSpec {
  val as = List(1, 2, 3, 4)
  val f = (x: Int) => x % 2 == 0
  "filter a list" should "success" in {
    val r = List.filter(as)(f)
    assert(r == List(2, 4))
  }

  "implement filter with foldRigth function" should "success" in {
    val r = List.filter2(as)(f)
    assert(r == List(2, 4))
  }
}
