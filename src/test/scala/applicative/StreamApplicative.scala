package test.fpscala.applicative

import fpscala.applicative.Applicative
import org.scalatest.FlatSpec
import fpscala.basic.Logger.Logger

class StreamApplicativeSpec extends FlatSpec {
  "test stream applicative sequence function" should "succeed" in {
    val as =
      List(Stream.continually(1), Stream.continually(2), Stream.continually(3))
    val r  = Applicative.streamApplicative.sequence(as)
    val rs = r.take(10).toList
    Logger.info(s"rs->$rs")
    succeed
  }
}
