package test.fpscala.errorhandling.either

import org.scalatest._
import fpscala.errorhandling._

class TraverseEithSpec extends FlatSpec {
  def traverse[E, A, B](as: List[A])(f: A => Either[E ,B]): Either[E, List[B]] = {
    as.foldRight[Either[E, List[B]]](Right(Nil))((x, y) => f(x).map2(y)(_ :: _))
  }

  "implement traverse function" should "success" in {
    val list = List(
      "1",
      "2",
      "3"
    )
    val r = traverse(list)
  }
}
