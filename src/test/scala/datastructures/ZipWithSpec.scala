package test.fpscala.datastructures

import org.scalatest._
import fpscala.datastructures._

class ZipWithSpec extends FlatSpec {
  val a = List(1, 2, 3)
  val b = List(4, 5, 6)

  val t = List(5, 7, 9)

  "test zipWith function" should "success" in {
    val r = List.zipWith(a,b)(_ + _)
    assert(r == t)
  }
}
