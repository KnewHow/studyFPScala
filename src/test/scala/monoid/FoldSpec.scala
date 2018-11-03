package test.fpscala.monoid

import org.scalatest.FlatSpec
import fpscala.monoid._
import prop.gen.Gen

class FoldSpec extends FlatSpec {
  "test foldLeft" should "succeed" in {
    val g = Gen.listOfN(Gen.choose(100, 1000), Gen.choose(0, 100000))
    val z = Gen.stringN(100)
    val f = (y: String, x: Int) => x + y
    val p = Fold.foldLeftLaw(g, z)(f)
    assert(p.test())
  }

  "test foldRight" should "succeed" in {
    val g = Gen.listOfN(10, Gen.choose(0, 10))
    val z = Gen.stringN(100).map(r => "ban")
    val f = (x: Int, y: String) => x + y
    val p = Fold.foldRightLaw(g, z)(f)
    assert(p.test())
  }
}
