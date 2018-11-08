package fpscala.monoid

trait Foldable[F[_]] {
  def foldLeft[A, B](as: F[A], z: B)(f: (B, A) => B): B
  def foldRight[A, B](as: F[A], z: B)(f: (A, B) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as, m.zero)(m.op)
  def toList[A](fa: F[A]): List[A]              = foldRight(fa, List[A]())(_ :: _)
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

sealed trait Tree[+A]
case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case object TreeFoldable extends Foldable[Tree] {
  def foldLeft[A, B](as: Tree[A], z: B)(f: (B, A) => B): B = as match {
    case Leaf(v)      => f(z, v)
    case Branch(l, r) => foldLeft(r, (foldLeft(l, z)(f)))(f)
  }
  def foldRight[A, B](as: Tree[A], z: B)(f: (A, B) => B): B = as match {
    case Leaf(v)      => f(v, z)
    case Branch(l, r) => foldRight(l, foldRight(r, z)(f))(f)

  }
  def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(v)      => mb.zero
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }
}

case object OptionFoldable extends Foldable[Option] {
  def foldLeft[A, B](as: Option[A], z: B)(f: (B, A) => B): B = as match {
    case None    => z
    case Some(a) => f(z, a)
  }
  def foldRight[A, B](as: Option[A], z: B)(f: (A, B) => B): B = as match {
    case None    => z
    case Some(a) => f(a, z)
  }
  def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None    => mb.zero
    case Some(a) => f(a)
  }
}
