package test.fpscala.localeffects

import org.scalatest.FlatSpec
import fpscala.localeffects._

class STSpec extends FlatSpec {
  "test ST run" should "succeed" in {
    val p = new RunnableST[(Int, Int)] {
      def apply[S]: ST[S, (Int, Int)] =
        for {
          r1 <- STRef(1)
          r2 <- STRef(2)
          x  <- r1.read
          y  <- r2.read
          _  <- r1.write(y + 1)
          _  <- r2.write(x + 1)
          a  <- r1.read
          b  <- r2.read
        } yield (a, b)
    }
    val (a, b) = ST.run(p)
    assert(a == 3 && b == 2)
  }
}
