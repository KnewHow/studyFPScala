package fpscala.monad

import fpscala.testing.Gen
import java.util.concurrent.ExecutorService
import fpscala.parallelism.NoBlockPar
import fpscala.state.State
trait Monad[F[_]] {
  def unit[A](a: => A): F[A]
  def compose[A, B, C](g: A => F[B], h: B => F[C]): A => F[C] =
    a => flatMap(g(a))(h)
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def _flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => fa, f)(())

  def map[A, B](fa: F[A])(f: A => B) = flatMap(fa)(a => unit(f(a)))
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C) = flatMap(fa) { a =>
    map(fb) { b =>
      f(a, b)
    }
  }
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(a => a)
  def sequence[A](lam: List[F[A]]): F[List[A]] =
    lam.foldRight[F[List[A]]](unit(List()))((a, b) => map2(a, b)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[A]): F[List[A]] =
    la.foldRight[F[List[A]]](unit(List()))((a, b) => map2(f(a), b)(_ :: _))

  def replicate[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)(_ -> _)

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight[F[List[A]]](unit(List()))((a, b) => {
      map2(
        product(f(a), unit(a)),
        b
      ) {
        case ((b, x), y) =>
          if (b) x :: y else y
      }
    })

  def _filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match {
    case Nil => unit(List())
    case h :: t =>
      flatMap(f(h))(b =>
        if (!b) _filterM(t)(f) else map2(unit(h), filterM(t)(f))(_ :: _))
  }

  def flatMapRaw[A, B, C, D](
    f: A => F[B],
    g: B => F[C],
    h: C => F[D]): A => F[Boolean] =
    a => {
      val r1 = compose(f, compose(g, h))
      val r2 = compose(compose(f, g), h)
      map2(r1(a), r2(a))(_ == _)
    }

  // def compose[G[_]](G: Monad[G]): Monad[({ type f[x] = F[G[x]] })#f] = {
  //   val self = this
  //   new Monad[({ type f[x] = F[G[x]] })#f] {
  //     def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
  //     override def flatMap[A,B](a:F[G[A]])(f: A => F[G[B]]) = {

  //     }
  //   }

  // }

}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A)                                 = Gen.unit(a)
    def flatMap[A, B](g: Gen[A])(f: A => Gen[B]): Gen[B] = g.flatMap(f)
  }

  type Par[A] = (ExecutorService) => fpscala.parallelism.Future[A]
  val parMonad = new Monad[Par] {
    def unit[A](a: => A)                         = NoBlockPar.unit(a)
    def flatMap[A, B](p: Par[A])(f: A => Par[B]) = NoBlockPar.flatMap(p)(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A)                               = Option(a)
    def flatMap[A, B](a: Option[A])(f: A => Option[B]) = a.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A)                               = Stream(a)
    def flatMap[A, B](s: Stream[A])(f: A => Stream[B]) = s.flatMap(f)
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A)                            = List(a)
    def flatMap[A, B](as: List[A])(f: A => List[B]) = as.flatMap(f)
  }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A) = State.unit(a)
    override def flatMap[A, B](s: State[S, A])(
      f: A => State[S, B]): State[S, B] = s.flatMap(f)
  }

  val idMonad = new Monad[Id] {
    def unit[A](a: => A)                               = Id(a)
    def flatMap[A, B](id: Id[A])(f: A => Id[B]): Id[B] = f(id.value)
  }

  def eitherMonad[E] = new Monad[({ type f[x] = Either[E, x] })#f] {
    def unit[A](a: => A)                                     = Right(a)
    def flatMap[A, B](a: Either[E, A])(f: A => Either[E, B]) = a flatMap (f)
  }
}
