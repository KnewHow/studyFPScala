package test.fpscala.datastructures

import org.scalatest._
import fpscala.datastructures._

class TreeSizeSpec extends BaseTreeSpec {
  "calculate tree size" should "success" in {
    val size = Tree.size(t)
    assert(size == 7)
  }
}
