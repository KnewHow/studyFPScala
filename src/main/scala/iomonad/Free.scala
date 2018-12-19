package fpscala.iomonad.free

import fpscala.parallelism.NoBlockPar
import fpscala.parallelism.NoBlockPar.Par
import fpscala.iomonad.Monad

sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))

}

case class Return[F[_], A](a: A) extends Free[F, A]

case class Suspend[F[_], A](fa: F[A]) extends Free[F, A]

case class FlatMap[F[_], A, B](fa: Free[F, A], k: A => Free[F, B])
    extends Free[F, B]

object Free {
  type Async[A] = Free[Par, A]

  type TailRec[A] = Free[Function0, A]

  def FreeMonad[F[_]]: Monad[({ type f[x] = Free[F, x] })#f] =
    new Monad[({ type f[x] = Free[F, x] })#f] {
      def unit[A](a: => A): Free[F, A] = Return(a)
      def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
        FlatMap(fa, f)
    }

  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = Translate[F, G]

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[A] = Free[G, A]
    val t = new (F ~> FreeG) {
      def apply[A](fa: F[A]): Free[G, A] = Suspend { fg(fa) }
    }
    run(f)(t)(freeMonad[G])
  }

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

  def step[F[_], A](fa: Free[F, A]): Free[F, A] = fa match {
    case FlatMap(Return(y), f) => step(f(y))
    case FlatMap(FlatMap(z, g), f) =>
      step(z flatMap (c => g(c) flatMap (b => f(b))))
    case _ => fa
  }

  def run[F[_], G[_], A](fa: Free[F, A])(t: F ~> G)(
    implicit G: Monad[G]): G[A] =
    step(fa) match {
      case Return(a)  => G.unit(a)
      case Suspend(r) => t.apply(r)
      case FlatMap(x, f) =>
        x match {
          case Suspend(y) => G.flatMap(t(y))(a => run(f(a))(t))
          case _          => sys.error("impossiable match for FlatMap")
        }
    }

}
