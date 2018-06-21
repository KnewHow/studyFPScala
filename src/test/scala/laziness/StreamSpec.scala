package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class StreamSpec extends FlatSpec {
  def expensive(x: Int) = {
    println("lala")
    x
  }

  "use origin constructor" should "execute twice expensive function" in {
    val r = Cons(() => expensive(3), () => Stream.empty)
    println("use origin constructor")
    r.headOption
    r.headOption
  }

  "user smart constructor" should "execute only once enpensive function" in {
    val r = Stream.cons(expensive(4), Stream.empty)
    println("use smart constructor")
    r.headOption
    r.headOption
  }
}
