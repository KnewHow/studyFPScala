package test.fpscala.datastructures

import org.scalatest._
import fpscala.datastructures._

class AddPairwiseSpec extends FlatSpec {
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case(_, Nil) => Nil
    case(Cons(h1,t1), Cons(h2, t2)) => Cons((h1+h2), addPairwise(t1, t2))
  }
  val a = List(1, 2, 3)
  val b = List(4, 5, 6)

  val t = List(5, 7, 9)

  "add two list element" should "success" in {
    val r = addPairwise(a,b)
    assert(r == t)
  }
}
