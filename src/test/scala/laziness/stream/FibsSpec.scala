package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class FibsSpec extends FlatSpec {
  def fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      Stream.cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  "test fibs function" should "success" in {
    val r = fibs.take(6).toList
    assert(r == List(0, 1, 1, 2, 3, 5))
  }

}
