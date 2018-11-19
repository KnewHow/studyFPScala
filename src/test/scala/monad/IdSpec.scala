package test.fpscala.monad

import org.scalatest.FlatSpec
import fpscala.monad.{Monad, Id}

class IdSpec extends FlatSpec {
  val id1 = Id[String]("Hello, ")
  val id2 = Id[String]("Monad")
  "test id monod" should "succeed" in {
    val r = for {
      a <- id1
      b <- id2
    } yield a + b
    assert(r == Id(id1.value + id2.value))
  }
}
