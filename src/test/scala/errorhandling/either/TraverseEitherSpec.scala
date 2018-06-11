package test.fpscala.errorhandling.either

import org.scalatest._
import fpscala.errorhandling._

class TraverseEithSpec extends FlatSpec {
  def traverse[E, A, B](as: List[A])(f: A => Either[E ,B]): Either[E, List[B]] = {
    as.foldRight[Either[E, List[B]]](Right(Nil))((x, y) => f(x).map2(y)(_ :: _))
  }

  def traverse_2[E, A, B](as: List[A])(f: A => Either[E ,B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverse_2(t)(f))(_ :: _)
  }

  def sequenceViaTraverse[E, A](as: List[Either[E, A]]): Either[E, List[A]] = traverse(as)(x => x)

  "implement traverse function with foldRigth function" should "success" in {
    val list = List(
      "1",
      "2",
      "3"
    )
    val r = traverse(list)(x => Either.Try(x.toInt))
    val t = Right(
      List(1, 2, 3)
    )
    println(r)
    assert(r == t)
  }

  "implemet traverse function with pattern match" should "success" in {
    val list = List(
      "1",
      "2",
      "3"
    )
    val r = traverse(list)(x => Either.Try(x.toInt))
    val t = Right(
      List(1, 2, 3)
    )
    println(r)
    assert(r == t)
  }

  "implement sequence function" should "success" in {
     val list = List(
      Right(1),
      // Left("first error"),
      Right(2)
      // Left("second error")
    )

    val r = sequenceViaTraverse(list)
    val t = Right(
      List(1, 2)
    )
    println(r)
    assert(r == t)
  }
}
