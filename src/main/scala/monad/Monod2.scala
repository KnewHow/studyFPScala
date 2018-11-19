package fpscala.monad

// use map unit and join rewrite monod
trait Monad2[F[_]] {
  def unit[A](a: => A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def join[A](mma: F[F[A]]): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def compose[A, B, C](g: A => F[B], h: B => F[C]): A => F[C] =
    a => flatMap(g(a))(h)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C) = flatMap(fa) { a =>
    map(fb) { b =>
      f(a, b)
    }
  }

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
}
