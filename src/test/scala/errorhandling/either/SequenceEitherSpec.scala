package test.fpscala.errorhandling.either

import org.scalatest._
import fpscala.errorhandling._

class SequenceEitherSpec extends FlatSpec {
  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] =
    a.foldRight[Either[E, List[A]]](Right(Nil))((x, y) => x.map2(y)(_ :: _))


  def sequence_2[E, A](a: List[Either[E, A]]): Either[E, List[A]] = a match {
    case Nil => Right(Nil)
    case h :: t => h flatMap(hh => sequence_2(t) map (tt => hh :: tt))
  }


  "implement either sequece" should "success" in {
    val list = List(
      Right(1),
      // Left("first error"),
      Right(2)
      // Left("second error")
    )

    val r = sequence_2(list)
    println(r)
    assert(true)
  }
}
