package fpscala.iomonad.free

import fpscala.parallelism.NoBlockPar
import fpscala.parallelism.NoBlockPar.Par
import fpscala.iomonad.Monad

trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))
}

case class Return[F[_], A](a: A) extends Free[F, A]

case class Suspend[F[_], A](fa: F[A]) extends Free[F, A]

case class FlatMap[F[_], A, B](fa: Free[F, A], k: A => Free[F, B])
    extends Free[F, B]

object Free {
  type Async[A]   = Free[Par, A]
  type TailRec[A] = Free[Function0, A]

  def freeMonad[F[_]]: Monad[({ type f[x] = Free[F, x] })#f] =
    new Monad[({ type f[x] = Free[F, x] })#f] {
      def unit[A](a: => A): Free[F, A]                      = Return(a)
      def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]) = FlatMap(fa, f)
    }

  def runTrampoline[A](fa: Free[Function0, A]): A = fa match {
    case Return(a)  => a
    case Suspend(f) => f.apply
    case FlatMap(x, f) =>
      x match {
        case Return(a)  => runTrampoline(f(a))
        case Suspend(g) => runTrampoline(f(g.apply))
        case FlatMap(y, g) =>
          runTrampoline(y flatMap (c => g(c) flatMap (b => f(b))))
      }
  }
}
