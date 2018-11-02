package test.fpscala.monoid

import org.scalatest.FlatSpec
import fpscala.monoid._
import prop.gen.Gen

class MoniodSpec extends FlatSpec {
  "test string monoid" should "succeed" in {
    val p = StringMoniod.law(Gen.stringN(100))
    assert(p.test())
  }

  "test list monoid" should "succeed" in {
    val p =
      ListMonoid().law(Gen.listOfN(Gen.choose(100, 1000), Gen.choose(0, 10000)))
    assert(p.test())
  }

  "test IntAddation monoid" should "succeed" in {
    val p = IntAddation.law(Gen.choose(0, 100))
    assert(p.test())
  }

  "test IntMultiplication" should "succeed" in {
    val p = IntMultiplication.law(Gen.choose(-1000, 10000))
    assert(p.test())
  }

  "test BooleanOr monoid" should "succeed" in {
    val p = BooleanOr.law(
      Gen.choose(0, 10000).map(r => r % 2 == 0)
    )
    assert(p.test())
  }

  "test BooleanAnd monoid" should "succeed" in {
    val p = BooleanAnd.law(
      Gen.choose(0, 10000).map(r => r % 2 == 0)
    )
    assert(p.test())
  }

  "test option monoid" should "succeed" in {
    val p = OptionMonoid().law(
      Gen.choose(0, 1000).map { r =>
        if (r % 10 == 0) {
          Some(r)
        } else {
          None
        }
      }
    )
    assert(p.test())
  }

  "test End monoid" should "succeed" in {
    val g = Gen.choose(0, 10000)
    val p = EndMonoid().law(
      g,
      g.map { r => (x: Int) =>
        x + 1
      }
    )
    assert(p.test())
  }
}
