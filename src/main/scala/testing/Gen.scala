package fpscala.testing

import fpscala.state.State
case class Gen[A](sample: State[RNG, A]) {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(RNG.nextInt(start, stopExclusive))

  def unit[A](a: => A): Gen[A] = Gen(State.unit[RNG, A](a))

  def boolean: Gen[Boolean] = Gen(RNG.boolean)

  def listOfN(n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap { n =>
    listOfN(n, this)
  }
}
