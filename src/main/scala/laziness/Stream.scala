package fpscala.laziness

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /**
   *  natural implement
   */
  def toListRecursive: List[A] = this match {
    case Empty => List.empty
    case Cons(h, t) => h() +: t().toListRecursive
  }

  /**
   * Above implement is not tail-recursive, if the list is too much, it will produce stack over flow. So we can use reverse
   * to implement this with tail-recurseive
   */
  def toListReverse: List[A] = {
    def go(s: Stream[A], l: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h():: l)
      case _  => l
    }
    go(this, List()).reverse
  }

  def toListFaster: List[A] = {
    var buffer = new collection.mutable.ListBuffer[A]
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) => {
        buffer += h()
        go(t())
      }
      case _ => buffer.toList
    }
    go(this)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n-1))
    case Cons(h, t) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => t().drop(n-1)
    case Cons(h, t) if n == 1 => t()
    case _ => Stream.empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if(p(h())) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def empty[A]: Stream[A] = Empty
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
