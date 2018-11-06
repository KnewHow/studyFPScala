package test.fpscala.monoid

import org.scalatest.FlatSpec
import fpscala.monoid._
import prop.gen.Gen

class FoldSpec extends FlatSpec {
  val g = Gen.listOfN(Gen.choose(1, 1000), Gen.choose(0, 10000))
  val z = Gen.stringN(100)

  "test foldLeft" should "succeed" in {
    val f = (y: String, x: Int) => x + y
    val p = Fold.foldLeftLaw(g, z)(f)
    assert(p.test())
  }

  "test foldRight" should "succeed" in {
    val f = (x: Int, y: String) => x + y
    val p = Fold.foldRightLaw(g, z)(f)
    assert(p.test())
  }

  "test foldRightViaFoldMapV" should "succeed" in {
    val f = (x: Int, y: String) => x + y
    val p = Fold.foldRightViaFoldMapVLaw(g, z)(f)
    assert(p.test())
  }

  "test foldLeftViaFoldMapV" should "succeed" in {
    val f = (y: String, x: Int) => x + y
    val p = Fold.foldLeftViaFoldMapVLaw(g, z)(f)
    assert(p.test())
  }

  "test foldLeftViaParFoldMap" should "succeed" in {
    val f = (y: String, x: Int) => x + y
    val p = Fold.foldLeftViaParFoldMapLaw(g, z)(f)
    assert(p.test())
  }

  "test foldRightViaParFoldMap" should "succeed" in {
    val f = (x: Int, y: String) => x + y
    val p = Fold.foldRightViaParFoldMapLaw(g, z)(f)
    assert(p.test())
  }
}
