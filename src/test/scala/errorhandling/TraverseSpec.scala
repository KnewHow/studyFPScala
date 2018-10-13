package test.fpscala.errorhandling

import org.scalatest._
import fpscala.errorhandling._

class TraverseSpec extends FlatSpec {

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((x, y) =>
      Option.map2(f(x), y)(_ :: _))

  def traverse_2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil    => Some(Nil)
      case h :: t => Option.map2(f(h), traverse_2(t)(f))(_ :: _)
    }
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  "implment traverse function with fold right" should "success" in {
    val list = List("1", "2", "3")
    val r    = traverse_2(list)((x: String) => Option.Try(x.toInt))
    println(r)
    assert(true)
  }

  "implement sequence function with traverse function" should "success" in {
    val list = List(
      Option(1),
      Option(2)
    )
    val r = sequenceViaTraverse(list)
    println(r)
    assert(true)
  }
}
