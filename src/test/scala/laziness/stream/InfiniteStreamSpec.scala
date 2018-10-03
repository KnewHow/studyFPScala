package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class InfiniteStreamSpec extends FlatSpec {
  val ones: Stream[Int] = Stream.cons(1, ones)

  "test infinite stream some function" should "success" in {
    println(ones.take(5).toList)
    println(ones.takeWhile(_ == 1))
    println(ones.forAll(_ != 1))
    // println(ones.forAll(_ == 1)) // Stack over flow
    assert(true)
  }

}
