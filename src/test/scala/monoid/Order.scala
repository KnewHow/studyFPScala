package test.fpscala.monoid

import org.scalatest.FlatSpec
import fpscala.monoid._
import prop.gen._

class OrderSpec extends FlatSpec {
  "test asc order function" should "succeed" in {
    val g = Gen.listOfN(Gen.choose(0, 1000), Gen.choose(-10000, 10000))
    val p = Prop.forAll(g) { s =>
      val r = s.sorted
      Fold.isOrderAsc(r.toIndexedSeq) && !Fold.isOrderAsc(s.toIndexedSeq)
    }
    assert(p.test())
  }
}
