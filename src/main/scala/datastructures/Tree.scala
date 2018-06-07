package fpscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](v: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](a: Tree[A]): Int  = a match {
    case Leaf(v) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

}
