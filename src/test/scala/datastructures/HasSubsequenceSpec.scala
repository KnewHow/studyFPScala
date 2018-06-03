package test.fpscala.datastructures

import fpscala.datastructures._
import org.scalatest._

class HasSubsequenceSpec extends FlatSpec {
  val as = List(1, 2, 3, 4)
  val s1 = List(1, 2)
  val s2 = List(2,3)
  val s3 = List(4)

  "test hasSubsequence function" should "success" in {
    val r1 = List.hasSubsequence(as,s1)
    val r2 = List.hasSubsequence(as,s2)
    val r3 = List.hasSubsequence(as,s3)
    println(s"r1 -> $r1")
    assert(r1 && r2 && r3)
  }
}
