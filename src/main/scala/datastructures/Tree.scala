package fpscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](v: A)                            extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](a: Tree[A]): Int = a match {
    case Leaf(v)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def equal[A](t1: Tree[A], t2: Tree[A]): Boolean = {
    (t1, t2) match {
      case (Leaf(v1), Leaf(v2)) if v1 == v2 => true
      case (Branch(l1, r1), Branch(l2, r2)) => equal(l1, l2) && equal(r1, r2)
      case _                                => false
    }
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(v)      => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

}
