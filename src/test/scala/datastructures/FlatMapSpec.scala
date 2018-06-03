package test.fpscala.datastructures

import org.scalatest._
import fpscala.datastructures._

class FlatMapSpec extends FlatSpec {
  val as =List(1, 2, 3)
  var t = List(1, 1, 2, 2, 3, 3)

  "test flatMap function implemented with append method" should "success" in {
    val r = List.flatMap(as)(i => List(i,i))
    assert(r == t)
  }

  "test flatMap function implemented with connect and map function" should "success" in {
    val r = List.flatMap2(as)(i => List(i, i))
    assert(r == t)
  }
}
