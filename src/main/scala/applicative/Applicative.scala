package fpscala.applicative
import fpscala.monad.Functor

trait Applicative[F[_]] extends Functor[F] {
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

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft[F[Map[K, V]]](unit(Map.empty)) {
      case (acc, (k, fv)) => {
        map2(fv, acc)((a, b) => {
          b + (k -> a)
        })
      }
    }

  def product[G[_]](
    G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] = {
    val self = this
    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
      def unit[A](a: => A) = (self.unit(a), G.unit(a))
      override def apply[A, B](f: (F[A => B], G[A => B]))(
        p: (F[A], G[A])): (F[B], G[B]) = (
        self.apply(f._1)(p._1) -> G.apply(f._2)(p._2)
      )
      override def map2[A, B, C](a: (F[A], G[A]), b: (F[B], G[B]))(
        f: (A, B) => C): (F[C], G[C]) = {
        apply(apply(unit(f.curried))(a))(b)
      }
    }
  }

  def compose[G[_]](
    G: Applicative[G]): Applicative[({ type f[x] = F[G[x]] })#f] = {
    val self = this
    new Applicative[({ type f[x] = F[G[x]] })#f] {
      def unit[A](a: => A) = self.unit(G.unit(a))
      override def map2[A, B, C](a: F[G[A]], b: F[G[B]])(
        f: (A, B) => C): F[G[C]] = {
        self.map2(a, b) { (ga, gb) =>
          G.map2(ga, gb)(f(_, _))
        }
      }
    }
  }
}

object Applicative {
  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] = Stream.continually(a)
    def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] =
      fa zip fb map (f.tupled)
  }
}
