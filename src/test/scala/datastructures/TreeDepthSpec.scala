package test.fpscala.datastructures

import org.scalatest._
import fpscala.datastructures._

class TreeDepthSpec extends BaseTreeSpec {
  "calculate tree depth" should "success" in {
    val d = Tree.depth(t1)
    assert(d == 4)
  }
}
