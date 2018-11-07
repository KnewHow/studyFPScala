package test.fpscala.monoid

import org.scalatest.FlatSpec
import fpscala.monoid._

class WCMonoid extends FlatSpec {
  "test monoid" should "succeed" in {
    val s = "You so beautiful, I fall in love with you."
    val r = WC.count(s)
    assert(r == 9)
  }
}
