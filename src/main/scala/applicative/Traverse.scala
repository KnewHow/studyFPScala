package fpscala.applicative

import fpscala.datastructures.{Tree, Leaf, Branch}
import fpscala.monad.{Functor}

trait Traverse[F[_]] extends Functor[F] {
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
