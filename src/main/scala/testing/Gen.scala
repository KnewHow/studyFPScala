package fpscala.testing

import fpscala.state.State
case class Gen[A](sample: State[RNG, A]) {
  def unit[A](a: => A): Gen[A] = Gen(State.unit[RNG, A](a))

  def boolean: Gen[Boolean] = Gen(RNG.boolean)

  def double: Gen[Double] = Gen(RNG.double)

  def listOfN(n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap { n =>
    listOfN(n, this)
  }
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Shreshould = g1._2 / (g1._2 + g2._2)
    Gen(RNG.double).flatMap(g => if (g > g1Shreshould) g1._1 else g2._1)
  }

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(RNG.nextInt(start, stopExclusive))
}
