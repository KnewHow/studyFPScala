package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class StreamBaseSpec extends FlatSpec {
  val s = Stream(
    { println("one"); 1 },
    { println("two"); 2 },
    { println("three"); 3 },
    { println("four"); 4 },
  )
}
