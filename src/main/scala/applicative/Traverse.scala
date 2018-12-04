package fpscala.applicative

import fpscala.datastructures.{Tree, Leaf, Branch}
import fpscala.monad.{Functor}
import fpscala.monoid.{Monoid, Foldable, EndMonoid}
import fpscala.state.State

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]: Applicative, A, B](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A)                           = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idMonad)

  type Const[M, B] = M
  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A) = M.zero
      def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M =
        M.op(m1, m2)
    }
  def foldLeft[A, M](as: F[A], z: M)(f: (M, A) => M): M =
    mapAccm(as, z)((a, b) => (a, f(b, a)))._2
  def foldRight[A, B](as: F[A], z: B)(f: (A, B) => B): B =
    mapAccm(as, z)((a, b) => (a, f(a, b)))._2
  def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({ type f[x] = Const[M, x] })#f, A, Nothing](as)(f)(mb)

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccm[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa) { a =>
      for {
        s1 <- State.get[S]
        (a2, s2) = f(a, s1)
        _ <- State.set(s2)
      } yield a2
    }.run(s)

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccm(fa, 0)((a, i) => ((a, i + 1), i + 1))._1

  override def toList[A](fa: F[A]): List[A] =
    mapAccm(fa, Nil: List[A])((a, as) => (a, a :: as))._2

  def reverse[A](fa: F[A]): F[A] =
    mapAccm(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](as: List[A])(f: A => G[B])(
      implicit G: Applicative[G]): G[List[B]] =
      as.foldLeft[G[List[B]]](G.unit(List()))((b, a) => G.map2(f(a), b)(_ :: _))
  }
  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(
      implicit G: Applicative[G]): G[Option[B]] = oa match {
      case Some(a) => G.map(f(a))(Some(_))
      case None    => G.unit(None)
    }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(
      implicit G: Applicative[G]): G[Tree[B]] =
      fa match {
        case Leaf(v) => G.map(f(v))(r => Leaf(r))
        case Branch(l, r) =>
          val r1 = traverse(l)(f)(G)
          val r2 = traverse(r)(f)(G)
          G.map2(r1, r2)(Branch(_, _))
      }
  }
}
