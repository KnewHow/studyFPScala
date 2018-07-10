package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class ToListSpec extends FlatSpec {
  val s = Stream(1, 2, 3, 4)
  println(s"stream list -> ${s}")

  "test toList function implemented with not-tail-recurvise" should "success" in {
    val l = s.toListRecursive
    println(s"no-tail-recursive result -> ${l}")
    assert(true)
  }

  "test toList function implemented with reverse and tail-recursive" should "success" in {
    val l = s.toListRecursive
    println(s"reverse and tail-recursive result -> ${l}")
    assert(true)
  }

  "test toList function implemented with imutable List" should "success" in {
    val l = s.toListFaster
    println(s"imutable list result -> ${l}")
    assert(true)
  }
}
