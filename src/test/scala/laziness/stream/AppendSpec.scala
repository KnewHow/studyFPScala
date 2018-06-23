package test.fpscala.laziness

import org.scalatest._
import fpscala.laziness._

class AppendSpec extends StreamBaseSpec {
  val s1 = Stream(
    {println("s1 one"); 1},
    {println("s1 two"); 2}
  )


  val s2 = Stream(
    {println("s2 three");3},
    {println("s2 four"); 4}
  )

  "test append function" should "success" in {
    val r = s1.append(s2)
    assert(r.toListFaster ==  List(1,2,3,4))
  }

}
