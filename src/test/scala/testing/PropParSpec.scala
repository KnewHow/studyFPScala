package test.fpscala.testing

import org.scalatest._
import fpscala.parallelism._
import java.util.concurrent._
import fpscala.testing._

class PropParSpec extends FlatSpec {
  "use origin tool test parallelism" should "success" in {
    val es: ExecutorService = Executors.newCachedThreadPool
    val p = SProp.forAll(SGen.unit(NoBlockPar.unit(1))) { r =>
      val p1 = NoBlockPar.map(r)(_ + 1)
      val p2 = NoBlockPar.unit(2)
      NoBlockPar.run(es)(p1) == NoBlockPar.run(es)(p2)
    }
    // p.test()
  }

  "use check function to prove" should "success" in {
    val es: ExecutorService = Executors.newCachedThreadPool
    val p = SProp.check {
      val p1 = NoBlockPar.map(NoBlockPar.unit(1))(_ + 1)
      val p2 = NoBlockPar.unit(2)
      NoBlockPar.run(es)(p1) == NoBlockPar.run(es)(p2)
    }
    // p.test()
  }

  "use forAllPar to prove unit(2) == unit(1).map(_ +1)" should "success" in {
    val sg = SGen.unit(NoBlockPar.unit(1))
    val p = SProp.forAllPar(sg) { r =>
      NoBlockPar.equals(
        NoBlockPar.map(r)(_ + 1),
        NoBlockPar.unit(2)
      )
    }
    // p.test(10, 10)
  }
  "Test Par fork function" should "success" in {
    val p = SProp.forAllPar(SGen.pint2) { r =>
      NoBlockPar.equals(
        NoBlockPar.fork(r),
        r
      )
    }
    p.test(10, 10)
  }
}
