package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class StartWithSpec extends FlatSpec {
  val s1 = Stream(1, 2, 3)
  val s2 = Stream(1, 2)

  val a1: Stream[Int] = Stream.cons(1, a1)
  val a2: Stream[Int] = Stream.cons(1, a2)

  "s1" should "start with s2" in {
    assert(s1.startWith(s2))
  }

  // "a2" should "start with a1" in {
  //   assert(a2.startWithViaZipAll(a1))
  // }

  "use zipAll implement startWith" should "success" in {
    assert(s1.startWithViaZipAll(s2))
  }
}
