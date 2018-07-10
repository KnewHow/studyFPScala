package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class UnfoldSpec extends FlatSpec {
  def fibs = Stream.unfold((0, 1)) { case (f0, f1) => Some(f0, (f1, f0 + f1)) }

  def ones = Stream.unfold(1) { case (i) => Some(1, 1) }

  def constant[A](a: A) = Stream.unfold(a)(_ => Some(a, a))

  def from(n: Int) = Stream.unfold(n) { case (i) => Some(i, i + 1) }

  "implement fibs with unfold" should "success" in {
    val r = fibs.take(6).toList
    assert(r == List(0, 1, 1, 2, 3, 5))
  }

  "implement ones with unfold" should "success" in {
    val r = ones.take(6).toList
    assert(r == List(1, 1, 1, 1, 1, 1))
  }

  "implement constant function with unfold" should "success" in {
    val r = constant(1).take(6).toList
    assert(r == List(1, 1, 1, 1, 1, 1))
  }

  "implement from function with unfold" should "success" in {
    val r = from(3).take(3).toList
    assert(r == List(3, 4, 5))
  }
}
