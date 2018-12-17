package fpscala.iomonad.async

import scala.annotation.tailrec
import fpscala.parallelism.NoBlockPar
import fpscala.parallelism.NoBlockPar.Par

object Async {

  @tailrec
  def step[A](as: Async[A]): Async[A] = as match {
    case FlatMap(Return(b), f) => step(f(b))
    case FlatMap(FlatMap(x, g), f) =>
      step(x flatMap (c => g(c) flatMap (b => f(b))))
    case _ => as
  }

  def run[A](as: Async[A]): Par[A] = step(as) match {
    case Return(a)  => NoBlockPar.unit(a)
    case Suspend(r) => r
    case FlatMap(x, f) =>
      x match {
        case Suspend(r) => NoBlockPar.flatMap(r)(a => run(f(a)))
        case _          => sys.error(s"Impossible step for FlatMap")
      }
  }

  sealed trait Async[A] {
    def flatMap[B](f: A => Async[B]): Async[B] = FlatMap(this, f)
    def map[B](f: A => B): Async[B]            = flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends Async[A]

  case class Suspend[A](pa: Par[A]) extends Async[A]

  case class FlatMap[A, B](as: Async[A], k: A => Async[B]) extends Async[B]
}
