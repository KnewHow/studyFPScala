package test.fpscala.datastructures

import fpscala.datastructures._
import org.scalatest._

class ConnectSpec extends FlatSpec {
  "connect a series list" should "success" in {
    val a = List(1)
    val b = List(2)
    val c = List(3)
    val d = List(4)
    val param = List(a, b, c, d)
    val r = List.connect(param)
    println(s"connect result -> $r")
    assert(r == List(1, 2, 3, 4))
  }
}
