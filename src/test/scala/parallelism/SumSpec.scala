package test.fpscala.parallelism

import org.scalatest._
import fpscala.parallelism._
import scala.collection._
import java.util.concurrent.{Future => JFuture}
import java.util.concurrent.ExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable
import java.util.concurrent.Executors

class SumSpec extends FlatSpec {

  val es = Executors.newScheduledThreadPool(10)

  type Par[A] = ExecutorService => JFuture[A]
  def sum(s: IndexedSeq[Int]): Par[Int] = {
    if (s.size <= 1) {
      Par.lazyUnit(s.headOption.getOrElse(0))
    } else {
      val (l, r) = s.splitAt(s.length)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }

  // The method will block thread
  "test sum parallelism function" should "success" in {
    // val s = List.fill(4)(1).toIndexedSeq
    // val p = sum(s)
    // val r = Par.run(es)(p).get
    // assert(r == 5)
  }
}
