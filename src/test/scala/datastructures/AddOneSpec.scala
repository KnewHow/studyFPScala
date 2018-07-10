package test.fpscala.datastructures

import org.scalatest._
import fpscala.datastructures._

class AddOneSpec extends FlatSpec {
  def addOne(as: List[Int]): List[Int] = {
    List.foldRigt(as, Nil: List[Int])((x, y) => Cons(x + 1, y))
  }
  val as = List(1, 2, 3)
  "add one for all elemenets in a list" should "success" in {
    val r = addOne(as)
    println(s"add one result -> $r")
    assert(r == List(2, 3, 4))
  }
}
