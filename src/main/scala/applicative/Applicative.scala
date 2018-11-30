package fpscala.applicative

trait Applicative[F[_]] {
  def unit[A](a: => A): F[A]
  def map2[A, B, C](a: F[A], b: F[B])(f: (A, B) => C): F[C]

  def map[A, B](a: F[A])(f: A => B): F[B] = map2(a, unit(Unit))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight[F[List[B]]](unit(List()))((a, b) => map2(f(a), b)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(a => a)

  def relicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A, B](a: F[A], b: F[B]): F[(A, B)] = map2(a, b)((a, b) => (a, b))

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((a, b) => a(b))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

}

object Applicative {
  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] = Stream.continually(a)
    def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] =
      fa zip fb map (f.tupled)

  }
}
