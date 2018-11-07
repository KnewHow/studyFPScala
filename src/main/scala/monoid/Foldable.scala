package fpscala.monoid

trait Foldable[F[_]] {
  def foldLeft[A, B](as: F[A], z: B)(f: (B, A) => B): B
  def foldRight[A, B](as: F[A], z: B)(f: (A, B) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as, m.zero)(m.op)
}

case class FoldableList[A]() extends Foldable[List] {
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(Monoid.dual(EndMonoid[B]()))(z)
  def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
}
