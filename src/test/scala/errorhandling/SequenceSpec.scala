package test.fpscala.errorhandling

import org.scalatest._
import fpscala.errorhandling._

class SequenceSpec extends FlatSpec {
  def map2[A, B, C](a:Option[A], b:Option[B])(f: (A, B) => C): Option[C] = {
    for{
      aa <- a
      bb <- b
    } yield {
      f(aa, bb)
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (tt => hh :: tt))
    }
  }

  def sequence_2[A](a: List[Option[A]]): Option[List[A]] ={
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x ,y)(_ :: _))
  }

  "test sequence" should "success" in {
    val list = List(
      Option(1),
      Option(2)
    )

    val r = sequence(list)
    println(r)
    assert(true)
  }
}
