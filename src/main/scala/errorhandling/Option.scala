package fpscala.errorhandling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(u) => Some(f(u))
      case None => None:Option[B]
    }
  }

  def getOrElse[B >: A](default: => B):B = {
    this match {
      case Some(u) => u
      case None => default
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def orElse[B >: A](default: => Option[B]): Option[B] = {
    map(Some(_)) getOrElse(default)
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(u) if(f(u)) => this
      case _ => None
    }
  }

}

case class Some[A](get: A) extends Option[A]

object None extends Option[Nothing]

object Option{
  def apply[A](a: A): Option[A] = {
    Some(a)
  }


  def map2[A, B, C](a:Option[A], b:Option[B])(f: (A, B) => C): Option[C] = {
    for{
      aa <- a
      bb <- b
    } yield {
      f(aa, bb)
    }
  }

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {case e: Exception => None}
  }

}
