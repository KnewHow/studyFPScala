package test.fpscala.datastructures

import org.scalatest._
import fpscala.datastructures._

class TreeFoldSpec extends BaseTreeSpec {
  def size[A](tr: Tree[A]):Int = {
    Tree.fold(tr)((x: A) => 1)((x:Int, y:Int) => 1 + x + y)
  }

  def maxinum(tr: Tree[Int]):Int = {
    Tree.fold(tr)((x: Int)=> x)((x:Int, y:Int) => x max y)
  }

  def depth[A](tr: Tree[A]): Int = {
    Tree.fold(tr)((x: A) => 1)((x: Int, y: Int) => 1+ (x max y))
  }

  def map[A,B](tr: Tree[A])(f: A => B) = {
    Tree.fold(tr)((x: A) => Leaf(f(x)): Tree[B])(Branch(_,_))
  }

  "use fold function implement size funciton" should "success" in {
    val r = size(t)
    assert(r == 7)
  }

  "use fold function implement maxinum function" should "success" in {
    val r = maxinum(t)
    assert(r == 4)
  }
  "use fold function implement depth function" should "success" in {
    val r = depth(t)
    assert(r == 3)
  }

  "use fold function implement map function" should "success " in {
    val left2 = Branch[String](Leaf("1"), Leaf("2"))
    val right2 = Branch[String](Leaf("3"), Leaf("4"))
    val t2 = Branch[String](left2,right2)
    val r = map(t)((x:Int) => x.toString)
    assert(Tree.equal(r, t2))
  }

}
