package test.fpscala.datastructures

import org.scalatest._
import fpscala.datastructures._

class MapSpec extends FlatSpec {
  "a List use map function" should "success" in {
    val f  = (x: Double) => x.toString
    val as = List(1.0, 2.0, 3.0)
    val r  = List.map(as)(f)
    println(s"map result -> $r")
    assert(r == List("1.0", "2.0", "3.0"))
  }
}
