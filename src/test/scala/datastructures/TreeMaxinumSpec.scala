package test.fpscala.datastructures

import org.scalatest._
import fpscala.datastructures._
import scala.math._

class TreeMaxinumSpec extends BaseTreeSpec {
  def maxinum(t: Tree[Int], m: Int): Int = {
    t match {
      case Leaf(v) => max(v, m)
      case Branch(l, r) => max(maxinum(l, m), maxinum(r, m))
    }
  }

  "obtain a tree max value" should "success" in {
    val mininum = -1234567
    val r = maxinum(t1,mininum)
    assert(r ==100)

  }
}
