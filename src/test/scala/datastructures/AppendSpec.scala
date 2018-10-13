package test.fpscala.datastructures

import org.scalatest._
import fpscala.datastructures._

class AppendSpec extends FlatSpec {
  val as           = List(1, 2, 3)
  val appendedList = List(1, 2, 3, 4)
  val b            = List(1, 2, 3, 4, 5)
  "append a element to a list" should "success" in {

    val r = List.appendWithFoldRight(as, 4)
    println(s"append result -> $r")
    assert(r == appendedList)
  }

  "append a list to a list" should "success" in {
    val s = List(4, 5)
    val r = List.append(as, s)
    assert(r == b)
  }

  "append a series list to a total list" should "success" in {
    val c1 = List(1)
    val c2 = List(2)
    val c3 = List(3)
    val r  = List.appendWithFoldRight(c1, c2, c3)
    println(s"series append result -> $r")
    assert(r == List(1, 2, 3))
  }
}
