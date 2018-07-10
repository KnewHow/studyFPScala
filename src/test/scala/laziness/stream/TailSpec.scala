package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class TailSpec extends FlatSpec {
  val s = Stream(1, 2, 3)

  val r = s.tails
  println(r.map(s => s.toList).toList)
  assert(
    r.toList.map { e =>
      e.toList
    } == List(
      List(1, 2, 3),
      List(2, 3),
      List(3),
    ))
  // assert(true)
}
