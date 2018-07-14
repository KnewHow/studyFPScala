package test.fpscala.parallelism

import org.scalatest._
import fpscala.parallelism._
import scala.collection._
import java.util.concurrent._

class SumSpec extends FlatSpec {

  val es = Executors.newScheduledThreadPool(10)

  type Par[A] = ExecutorService => Future[A]
  def sum(s: IndexedSeq[Int]): Par[Int] = {
    if (s.size <= 1) {
      Par.lazyUnit(s.headOption.getOrElse(0))
    } else {
      val (l, r) = s.splitAt(s.length)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }

  "test sum parallelism function" should "success" in {
    val s = List.fill(4)(1).toIndexedSeq
    val p = sum(s)
    val r = Par.run(es)(p).get
    assert(r == 5)
  }
}
