package fpscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
  def foldRigt[A, B](list: List[A], z: B)(f: (A, B) => B): B = {
    list match {
      case Nil => z
      case Cons(h, t) => f(h, foldRigt(t, z)(f))
    }
  }

  def foldLeft[A,B](as: List[A], z: B)(f:(B, A) => B):B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def reverse[A](as: List[A]) :List[A] = {
    foldLeft(as, Nil:List[A])((b, a) => Cons(a, b))
  }

  def foldRigtViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))
  }

  def appendWithFoldRight[A](as: List[A], e: A): List[A] = {
    foldRigt(as,Cons(e, Nil: List[A]))(Cons(_,_))
  }

  def append[A](a: List[A], b: List[A]): List[A] = {
    foldRigt(a,b)(Cons(_,_))
  }

  def appendWithFoldRight[A](a: List[A]*): List[A] = {
    var f = Nil:List[A]
    for(e <- a) {
      f = append(f,e)
    }
    f
  }

  def connect[A](as: List[List[A]]): List[A] = {
    foldRigt(as, Nil: List[A])(append)
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRigt(as,Nil: List[B])((x,y) => Cons(f(x),y))
  }


  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRigt(as, Nil:List[B])((x,y) => append(f(x),y))
  }

  def flatMap2[A,B](as: List[A])(f: A => List[B]): List[B] = {
    connect(map(as)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case Cons(x, h) => if(f(x)) Cons(x, filter(h)(f)) else filter(h)(f)
    }
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRigt(as, Nil: List[A])((x,y) => if(f(x)) Cons(x,y) else y)
  }

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(i => if(f(i)) List(i) else Nil: List[A])
  }

  def zipWith[A,B,C](a: List[A], b: List[B])(f:(A,B) => C): List[C] = (a,b) match {
    case(Nil,_) => Nil
    case(_,Nil) => Nil
    case(Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

}
