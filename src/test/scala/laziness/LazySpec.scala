package test.fpscala.laziness

import org.scalatest._

class LazySpec extends FlatSpec {
  def sum(a: Int, b: Int): Int = a + b
  def sumLazy(a: Int, b: => Int): Int = a

  val a = 3
  val b = 4
  val lazyB = () => {
    println("generate b")
    4
  }

  "test unlazy sum" should "success" in {
    val r = sum(a, b)
    assert(r == 7)
  }

  "test lazy sum" should "success" in {
    val r = sumLazy(a, lazyB())
    assert(r == 3)
  }
}
