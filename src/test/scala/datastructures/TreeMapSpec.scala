package test.fpscala.datastructures

import org.scalatest._
import fpscala.datastructures._

class TreeMapSpec extends BaseTreeSpec {
  def add(t: Tree[String]): Unit = t match {
    case Leaf(v)      => println(v + 1)
    case Branch(l, r) => { add(l); add(r) }
  }
  "test tree map function" should "success" in {
    val left2  = Branch[String](Leaf("1"), Leaf("2"))
    val right2 = Branch[String](Leaf("3"), Leaf("4"))
    val t2     = Branch[String](left2, right2)
    val r      = Tree.map(t)((x: Int) => x.toString)
    assert(Tree.equal(r, t2))
  }
}
