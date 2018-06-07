package test.fpscala.datastructures

import fpscala.datastructures._
import org.scalatest._

class BaseTreeSpec extends FlatSpec {
  val left = Branch(Leaf(1), Leaf(2))
  val right = Branch(Leaf(3), Leaf(4))
  val t = Branch(left,right)

  val l1 = Branch(Leaf(1),Branch(Leaf(100),Leaf(60)))
  val t1 = Branch(l1, right)
}
