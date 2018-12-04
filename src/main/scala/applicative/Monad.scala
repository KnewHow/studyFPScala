package fpscala.applicative

import fpscala.state.State

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B) = flatMap(fa)(a => unit(f(a)))
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C) = flatMap(fa) { a =>
    map(fb) { b =>
      f(a, b)
    }
  }
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(a => a)

  def compose[A, B, C](g: A => F[B], h: B => F[C]): A => F[C] =
    a => flatMap(g(a))(h)
}

object Monad {
  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A) = State.unit(a)
    override def flatMap[A, B](fa: State[S, A])(
      f: A => State[S, B]): State[S, B] = fa.flatMap(f)
  }
}
