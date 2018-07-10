package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class TakeSpec extends FlatSpec {
  val s = Stream(1, 2, 3, 4)

  "test take function" should "success" in {
    val r = s.take(3)
    println(s"result -> ${r.toListFaster}")
    assert(r.toListFaster == List(1, 2, 3))
  }

  "test drop function" should "succcess" in {
    val r = s.drop(2)
    assert(r.toListFaster == List(3, 4))
  }

  "test takeWhile function" should "success" in {
    val r = s.takeWhile((i: Int) => i < 3)
    assert(r.toListFaster == List(1, 2))
  }
}
