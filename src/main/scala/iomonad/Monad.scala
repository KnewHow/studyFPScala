package fpscala.iomonad

import fpscala.monad.Id
import fpscala.testing.Gen
import java.util.concurrent.ExecutorService
import fpscala.parallelism.NoBlockPar
import fpscala.state.State

trait Monad[F[_]] { self =>
  def unit[A](a: => A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def compose[A, B, C](g: A => F[B], h: B => F[C]): A => F[C] =
    a => flatMap(g(a))(h)
  def _flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => fa, f)(())
  def map[A, B](fa: F[A])(f: A => B) = flatMap(fa)(a => unit(f(a)))
  def as[A, B](fa: F[A])(b: B): F[B] = fa map (_ => b)
  def skip[A](fa: F[A]): F[Unit]     = fa as (Unit)
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

  def replicate[A](n: Int)(ma: => F[A]): F[List[A]] = sequence(List.fill(n)(ma))

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

  // repeat F[A] until cond return false
  def doWhile[A](fa: F[A])(cond: A => F[Boolean]): F[Unit] =
    for {
      a  <- fa
      ok <- cond(a)
      _  <- if (ok) doWhile(fa)(cond) else unit(())
    } yield ()

  // do repeat
  def forever[A, B](fa: F[A]): F[B] = {
    lazy val fb: F[B] = forever(fa)
    fa flatMap (_ => fb)
  }

  def foldM[A, B](s: Stream[A])(z: B)(f: (B, A) => F[B]): F[B] = s match {
    case h #:: t => f(z, h) flatMap (b => foldM(t)(b)(f))
    case _       => unit(z)
  }

  def foldM_[A, B](s: Stream[A])(z: B)(f: (B, A) => F[B]): F[Unit] =
    foldM(s)(z)(f) skip

  def foreachM[A, B](s: Stream[A])(f: A => F[Unit]): F[Unit] =
    foldM_(s)(Unit)((_, a) => skip(f(a)))

  // a implict operator of F[A], make code more elegant
  implicit def FtoFOps[A](fa: F[A]): FOps[A] = FOps(fa)
  case class FOps[A](fa: F[A]) {
    def map[B](f: A => B)        = self.map(fa)(f)
    def flatMap[B](f: A => F[B]) = self.flatMap(fa)(f)
    def as[B](b: B): F[B]        = self.map(fa)(_ => b)
    def skip                     = self.skip(fa)
  }

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
