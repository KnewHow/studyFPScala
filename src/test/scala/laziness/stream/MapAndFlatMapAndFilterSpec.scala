package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class MapAndFlatMapAndFilterSpec extends StreamBaseSpec {
  "test map function implemented with foldRight" should "success" in {
    val r = s.map(i => i.toString)
    assert(r.toListFaster == List("1", "2", "3", "4"))
  }

  "test flatMap function" should "success" in {
    val r = s.flatMap(a => Stream(a.toString))
    assert(r.toListFaster == List("1", "2", "3", "4"))
  }

  "test filter function" should "success" in {
    val r = s.filter(i => i % 2 == 0)
    assert(r.toListFaster == List(2, 4))
  }
}
