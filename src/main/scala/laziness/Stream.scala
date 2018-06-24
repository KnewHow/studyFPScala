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
  def toList: List[A] = {
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
    case Cons(h, t)  => if(p(h())) Stream.cons(h(), t().takeWhile(p)) else Stream.empty
    case _ => Stream.empty
  }

  def foldRight[B](z: B)(f:(A, =>B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def originalExists(f: A => Boolean): Boolean = this match {
    case Cons(h, t) => f(h()) || t().originalExists(f)
    case _ => false
  }

  def foldRightExists(f: A => Boolean): Boolean = this.foldRight(false)((a, b) => f(a) || b)

  def forAll(f: A => Boolean): Boolean = this.foldRight(true)((a, b) => f(a) && b)

  def takeWhileFoldRight(f: A => Boolean): Stream[A] = this.foldRight[Stream[A]](Stream.empty)((a, b) =>
    if(f(a)) Stream.cons(a,b)
    else Stream.empty)

  def headOptionFoldRight: Option[A] = this.foldRight(None:Option[A])((a,b) => Some(a))

  def map[B](f: A => B): Stream[B] = this.foldRight[Stream[B]](Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def append[B >: A](a: => Stream[B]) = this.foldRight[Stream[B]](a)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight[Stream[B]](Stream.empty[B])((a, b) => f(a).append(b))

  def filter(f: A => Boolean): Stream[A] = this.foldRight[Stream[A]](Stream.empty)((a, b) => if(f(a)) Stream.cons(a, b) else b)

  def mapViaUnFold[B](f: A => B): Stream[B] =
    Stream.unfold(this)(i => i.headOption.flatMap(r => Some(f(r),i.drop(1))))

  def mapViaUnFold2[B](f: A => B): Stream[B] =
    Stream.unfold[B, Stream[A]](this){
     i =>  i match {
        case Cons(h, t) => Some(f(h()), t())
        case _ => None
      }
    }
  def mapViaUnFold3[B](f: A => B): Stream[B] =
    Stream.unfold[B, Stream[A]](this){
        case Cons(h, t) => Some(f(h()), t())
        case _ => None
    }

  def takeViaUnFold(n: Int): Stream[A] =
    Stream.unfold((this, n)){
      case (Cons(h, t), i)  if(i > 1) => Some(h(),(t(), i-1))
      case (Cons(h, t), i)  if(i == 1) => Some(h(),(Stream.empty, i-1))
      case _ => None
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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]):Stream[A] = f(z) match {
    case Some((h, t)) => cons(h, unfold(t)(f))
    case _ => empty
  }

}
