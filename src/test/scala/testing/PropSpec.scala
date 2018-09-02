package test.fpscala.testing
import fpscala.testing._
import org.scalatest._

class PropSpec extends FlatSpec {
  val g = Gen(RNG.nextInt(1, 10))
  val rng = RNG(42)
  "test Prop forAll function" should "success" in {
    val prop = Prop.forAll[Int](g)(a => (a >= 2 && a < 10))
    // prop.check(100, rng)
  }

  "test && function" should "success" in {
    val p1 = Prop.forAll(g)(_ > 1)
    val p2 = Prop.forAll(g)(_ < 8)
    val p = p1 && p2
    // p.check(100, rng)
  }

  "test || function" should "success" in {
    val p1 = Prop.forAll(g)(_ > 1)
    val p2 = Prop.forAll(g)(_ < 8)
    val p = p1 || p2
    // p.check(100, rng)
  }

  "test listOfN function" should "success" in {
    val g2 = Gen(RNG.nextInt(1, 1000))
    val gList = g.listOfN(g2)
    val prop = Prop.forAll(gList) { r =>
      val max = r.max
      !r.exists(_ > max)
    }
    prop.test(100, rng)
  }
}
