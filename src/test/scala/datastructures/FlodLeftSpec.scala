package test.fpscala.datastructures

import org.scalatest._

import fpscala.datastructures._

class FoldLeftSpec extends FlatSpec {
  val intList = List(1, 2, 3)
  val doubleList = List(1.0, 2.0, 3.0)
  def sum(as: List[Int]): Int = {
    List.foldLeft(as, 0)(_ + _)
  }

  def product(as: List[Double]): Double = {
    List.foldLeft(as, 1.0)(_ * _)
  }

  def length[A](as: List[A]): Int = {
    List.foldLeft(as, 0)((x, y) => x + 1)
  }

  "implement sum method with foldLeft" should "same as real sum" in {
    val r = sum(intList)
    assert(r == 6)
  }

  "implement product function with fold left" should "same as real product" in {
    val r = product(doubleList)
    assert(r == 6.0)
  }

  "implement length function with fold left" should "same as real length" in {
    val r = length(intList)
    assert(r == 3)
  }
}
