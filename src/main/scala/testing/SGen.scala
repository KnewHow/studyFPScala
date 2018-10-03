package fpscala.testing

import fpscala.parallelism._
import java.util.concurrent._

case class SGen[A](forSize: Int => Gen[A]) {

  def boolean: SGen[Boolean] = SGen(forSize(_).boolean)

  def double: SGen[Double] = SGen(forSize(_).double)

  def listOfN(n: Int, g: SGen[A]): SGen[List[A]] = {
    SGen(r => forSize(r).listOfN(n, g.forSize(r)))
  }

  def map[B](f: A => B): SGen[B] = SGen(r => forSize(r).map(f))

  def listOfN(size: SGen[Int]): SGen[List[A]] = size.flatMap { n =>
    listOfN(n, this)
  }

  def listOf1(size: SGen[Int]): SGen[List[A]] =
    size.flatMap(n => listOfN((n max 1), this))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(r => forSize(r).flatMap(a => f(a).forSize(r)))

  def union(g1: SGen[A], g2: SGen[A]): SGen[A] =
    boolean.flatMap(a => if (a) g1 else g2)

  def map2[B, C](g: SGen[B])(f: (A, B) => C): SGen[C] =
    SGen(r => forSize(r).map2(g.forSize(r))(f))

  def **[B](g: SGen[B]): SGen[(A, B)] = this.map2(g)(_ -> _)
}

object SGen {
  def double: SGen[Double] = SGen(r => Gen.double)
  def choose(start: Int, stopExclusive: Int): SGen[Int] =
    SGen(r => Gen.choose(start, stopExclusive))
  def unit[A](a: => A): SGen[A] = SGen(r => Gen.unit(a))

  def weighted[A](g1: (SGen[A], Double), g2: (SGen[A], Double)): SGen[A] = {
    val g1Shreshould = g1._2 / (g1._2 + g2._2)
    double.flatMap(r => if (r > g1Shreshould) g1._1 else g2._1)
  }
  type Par[A] = (ExecutorService) => fpscala.parallelism.Future[A]
  def pint2: SGen[Par[Int]] = SGen(r => Gen.pint2)
}
