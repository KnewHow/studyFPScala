package fpscala.testing

case class SGen[A](forSize: Int => Gen[A]) {
  def unit[A](a: => A): SGen[A] = SGen(forSize(_).unit(a))

  def boolean: SGen[Boolean] = SGen(forSize(_).boolean)

  def double: SGen[Double] = SGen(forSize(_).double)

  def listOfN(n: Int, g: SGen[A]): SGen[List[A]] = {
    SGen(r => forSize(r).listOfN(n, g.forSize(r)))
  }

  def map[B](f: A => B): SGen[B] = SGen(r => forSize(r).map(f))

  def listOfN(size: SGen[Int]): SGen[List[A]] = size.flatMap { n =>
    listOfN(n, this)
  }

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(r => forSize(r).flatMap(a => f(a).forSize(r)))

  def union(g1: SGen[A], g2: SGen[A]): SGen[A] =
    boolean.flatMap(a => if (a) g1 else g2)

  def weighted[A](g1: (SGen[A], Double), g2: (SGen[A], Double)): SGen[A] = {
    val g1Shreshould = g1._2 / (g1._2 + g2._2)
    double.flatMap(r => if (r > g1Shreshould) g1._1 else g2._1)
  }

}

object SGen {
  def choose(start: Int, stopExclusive: Int): SGen[Int] =
    SGen(r => Gen.choose(start, stopExclusive))
}
