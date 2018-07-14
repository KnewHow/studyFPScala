package test.fpscala.parallelism

import org.scalatest._
import fpscala.parallelism._
import java.util.concurrent.{Executors, ExecutorService}

class SumNoblockSpec extends FlatSpec {
  val es = Executors.newFixedThreadPool(10)

  type Par[A] = (ExecutorService) => Future[A]

  def sum(s: IndexedSeq[Int]): Par[Int] = {
    if (s.size <= 1) {
      NoBlockPar.unit(s.headOption.getOrElse(0))
    } else {
      val (l, r) = s.splitAt(s.size / 2)
      NoBlockPar.map2(NoBlockPar.fork(sum(l)), NoBlockPar.fork(sum(r)))(_ + _)
    }
  }

  "parallelism sum" should "same with orgin sum" in {
    val l = List.fill(10000)(1).toIndexedSeq
    val p = sum(l)
    val r = NoBlockPar.run(es)(p)
    assert(r == l.sum)
  }
}
