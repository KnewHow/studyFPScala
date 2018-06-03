package test.fpscala.datastructures

import org.scalatest._
import fpscala.datastructures._

class Double2StringSpec extends FlatSpec {
  def double2String(as: List[Double]): List[String] = {
    List.foldRigt(as, Nil: List[String])((x, y) => Cons(x.toString,y))
  }

  val as: List[Double] = List(1.0, 2.0, 3.0)

  "double list transform string list" should "success" in {
    val r = double2String(as)
    println(s"double to string result ->$r")
    assert(r == List("1.0", "2.0", "3.0"))
  }
}
