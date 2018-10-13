package test.fpscala.testing

import org.scalatest._
import fpscala.testing._
import fpscala.parallelism.Par

class SPropSpec extends FlatSpec {
  "test SProp forAll function" should "success" in {
    val sg = SGen.choose(1, 10)
    val p  = SProp.forAll(sg)(_ < 10)
    // p.check()
  }
  "use SProp test reverse function" should "success" in {
    val size  = SGen.choose(1, 1000)
    val sg    = SGen.choose(1, 10000)
    val gList = sg.listOfN(size)
    val p = SProp.forAll(gList) { r =>
      r.reverse.reverse == r && r.headOption == r.reverse.lastOption
    }
    // p.check()
  }

  "use SProp test max function" should "success" in {
    val size  = SGen.choose(1, 10)
    val sg    = SGen.choose(1, 100)
    val gList = sg.listOfN(size)
    val p = SProp.forAll(gList) { r =>
      val max = r.max
      !r.exists(_ > max)
    }
    // p.check()
  }
  "use SProp test sorted function" should "success" in {
    val size  = SGen.choose(1, 1000)
    val sg    = SGen.choose(1, 1000)
    val gList = sg.listOfN(size)
    val p = SProp.forAll(gList) { r =>
      val min = r.sorted.headOption.get
      !r.exists(_ < min)
    }
    p.test()
  }
}
