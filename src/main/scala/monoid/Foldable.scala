package fpscala.monoid

trait Foldable[F[_]] {
  def foldLeft[A, B](as: F[A], z: B)(f: (B, A) => B): B
  def foldRight[A, B](as: F[A], z: B)(f: (A, B) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as, m.zero)(m.op)
}

case object ListFoldable extends Foldable[List] {
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(Monoid.dual(EndMonoid[B]()))(z)
  def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
}

case object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  def foldLeft[A, B](as: IndexedSeq[A], z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
  def foldRight[A, B](as: IndexedSeq[A], z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    Fold.foldMapV(as, mb)(f)
}

case object StreamFoldable extends Foldable[Stream] {
  def foldLeft[A, B](as: Stream[A], z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  def foldRight[A, B](as: Stream[A], z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
}
