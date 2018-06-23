package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class HeadOptionSpec extends FlatSpec {
  val s = Stream(1, 2, 3, 4)
  "implement headOption function with foldRight" should "success" in {
    val r = s.headOptionFoldRight
    assert(r == Some(1))
  }
}
