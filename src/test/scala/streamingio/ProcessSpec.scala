package test.fpscala.streamingio

import org.scalatest.FlatSpec
import fpscala.streamingio.Process

class ProcessSpec extends FlatSpec {
  "test liftOne" should "succeed" in {
    def f = (x: Int) => x * 2
    val p = Process.liftOne(f)
    val r = p(Stream(1, 2, 3)).toList
    assert(r == List(2))
  }

  "test lift" should "succeed" in {
    def f = (x: Int) => x * 2
    val p = Process.lift(f)
    val r = p(Stream(1, 2, 3)).toList
    assert(r == List(2, 4, 6))
  }

  "test filter" should "succeed" in {
    def f = (x: Int) => x + 1
    // val even = Process.lift(f).filter(x => x % 2 == 0)
    val even = Process.filter[Int](x => x % 2 == 0)
    val r    = even(Stream(1, 2, 3, 4)).toList
    assert(r == List(2, 4))
  }

  "test sum" should "succeed" in {
    def f   = (x: Int) => x * 2
    val sum = Process.lift(f).sum
    val r   = sum(Stream(1, 2, 3)).toList
    assert(r == List(1.0, 3.0, 6.0))
  }

  "test count" should "succeed" in {
    val r = Process.count(Stream("aa", "bb", "ccc")).toList
    assert(r == List(1, 2, 3))

  }
}
