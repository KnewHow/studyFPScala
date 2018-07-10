package test.fpscala.datastructures

import org.scalatest._
import fpscala.datastructures._

class FoldRightSpec extends FlatSpec {

  val intList = List(1, 2, 3)
  def sum(list: List[Int]): Int = {
    List.foldRigt(list, 0)(_ + _)
  }

  def length[A](as: List[A]): Int = {
    List.foldRigt[A, Int](as, 0)((x, y) => y + 1)
  }

  "test sum function with foldRight" should "successful" in {

    val r = sum(intList)
    assert(r == 6)
  }

  "use flodRight implement" should "same as real length" in {
    val r = length(intList)
    assert(r == 3)
  }
}
