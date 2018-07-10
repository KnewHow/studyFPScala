package test.fpscala.datastructures

import org.scalatest._
import fpscala.datastructures._
import scala.math._

class TreeMaxinumSpec extends BaseTreeSpec {
  def maxinum(t: Tree[Int]): Int = {
    t match {
      case Leaf(v)      => v
      case Branch(l, r) => maxinum(l) max maxinum(r)
    }
  }

  "obtain a tree max value" should "success" in {
    val r = maxinum(t1)
    assert(r == 100)

  }
}
