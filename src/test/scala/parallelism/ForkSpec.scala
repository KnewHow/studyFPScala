package test.fpscala.parallelism

import org.scalatest._
import fpscala.parallelism._
import java.util.concurrent._

class ForkSpec extends FlatSpec {
  val a = Par.lazyUnit(42 + 1)
  val s = Executors.newFixedThreadPool(9)
  assert(Par.equals(s)(a, Par.fork(a)))
}
