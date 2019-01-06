package fpscala.streamingio

import fpscala.applicative.Monad
import fpscala.iomonad.io2.IO
import fpscala.basic.Logger.Logger
import scala.annotation.tailrec

sealed trait Process[I, O] {

  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) =>
      s match {
        case h #:: t => recv(Some(h))(t)
        case xs      => recv(None)(xs)
      }
    case Emit(h, t) => h #:: t(s)
  }

  def liftOne[I, O](f: I => O): Process[I, O] = Await {
    case Some(i) => Emit(f(i))
    case None    => Halt()
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) =>
        Await {
          case None => recv(None)
          case i    => go(recv(i))
        }
      case Emit(h, t) => Emit(h, go(t))
    }
    go(this)
  }

  def lift[I, O](f: I => O) = liftOne(f).repeat

  def emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()): Process[I, O] =
    Emit(head, tail)

  def filter[I](f: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(i) if f(i) => emit(i)
      case _               => Halt()
    }.repeat

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] = Await {
      case Some(d) => Emit(acc + d, go(acc + d))
      case None    => Halt()
    }
    go(0.0)
  }

  def await[I, O](
    f: I => Process[I, O],
    fallback: Process[I, O] = Halt[I, O]()) = Await[I, O] {
    case Some(i) => f(i)
    case None    => fallback
  }

  def id[I]: Process[I, I] =
    await((i: I) => emit(i, id))

  def take[I](n: Int): Process[I, I] =
    if (n <= 0)
      Halt()
    else
      await(i => emit(i, take(n - 1)))

  def drop[I](n: Int): Process[I, I] =
    if (n <= 0) id
    else await(i => drop(n - 1))

  def takeWhile[I](f: I => Boolean): Process[I, I] =
    await(
      i =>
        if (f(i)) emit(i, takeWhile(f))
        else Halt())

  def dropWhile[I](f: I => Boolean): Process[I, I] =
    await(
      i =>
        if (f(i)) dropWhile(f)
        else emit(i, id))

  def count: Process[I, Int] = {
    def go(n: Int): Process[I, Int] = await(i => emit(n + 1, go(n + 1)))
    go(0)
  }

  def mean: Process[Double, Double] = {
    def go(sum: Double, count: Double): Process[Double, Double] =
      await(
        (d: Double) =>
          emit(
            (sum + d) / (count + 1),
            go(sum + d, count + 1)
        ))
    go(0.0, 0.0)
  }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] = await(
    (i: I) =>
      f(i, z) match {
        case (o, s2) => emit(o, loop(s2)(f))
    }
  )

  def sumViaLoop = loop(0.0)((i: Double, s) => (i + s, i + s))

  def countViaLoop: Process[I, Int] = loop(0)((i: I, c) => (c + 1, c + 1))

  def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match {
    case Halt()     => Halt()
    case Emit(h, t) => Emit(h, this |> t)
    case Await(recv) =>
      this match {
        case Halt()       => Halt() |> recv(None)
        case Emit(h, t)   => t |> recv(Some(h))
        case Await(recv2) => Await((i: Option[I]) => recv2(i) |> p2)
      }
  }

  def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

  def ++(p: => Process[I, O]): Process[I, O] = this match {
    case Halt()      => p
    case Emit(h, t)  => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt()      => Halt()
    case Emit(h, t)  => f(h) ++ t.flatMap(f)
    case Await(recv) => Await(recv andThen (_ flatMap f))
  }

  def monad[I]: Monad[({ type f[x] = Process[I, x] })#f] =
    new Monad[({ type f[x] = Process[I, x] })#f] {
      def unit[O](o: => O): Process[I, O]                          = Emit(o, Halt())
      def flatMap[O, O2](p: Process[I, O])(f: O => Process[I, O2]) = p flatMap f
    }

  def zipWithIndex: Process[I, (O, Int)] = zip(this, count map (_ - 1))

  def feed[A, B](oa: Option[A])(p: Process[A, B]): Process[A, B] = p match {
    case Halt()     => Halt()
    case Emit(h, t) => Emit(h, feed(oa)(t))
    case Await(recv) =>
      recv(oa)
  }

  def zip[A, B, C](p1: Process[A, B], p2: Process[A, C]): Process[A, (B, C)] =
    (p1, p2) match {
      case (Halt(), _)                => Halt()
      case (_, Halt())                => Halt()
      case (Emit(b, t1), Emit(c, t2)) => Emit((b, c), zip(t1, t2))
      case (Await(recv1), _) =>
        Await((oa: Option[A]) => zip(recv1(oa), feed(oa)(p2)))
      case (_, Await(recv2)) =>
        Await((oa: Option[A]) => zip(feed(oa)(p1), recv2(oa)))
    }

  def exists[I](f: I => Boolean): Process[I, Boolean] = lift(f) |> any

  def any[I]: Process[Boolean, Boolean] =
    loop(false)((b: Boolean, s) => (b || s, b || s))
}

case class Emit[I, O](
  head: O,
  tail: Process[I, O] = Halt[I, O]()
) extends Process[I, O]

case class Await[I, O](
  recv: Option[I] => Process[I, O]
) extends Process[I, O]

case class Halt[I, O]() extends Process[I, O]

object Process {
  def liftOne[I, O](f: I => O): Process[I, O] = Await {
    case Some(i) => Emit(f(i))
    case None    => Halt()
  }

  def lift[I, O](f: I => O) = liftOne(f).repeat

  def emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()): Process[I, O] =
    Emit(head, tail)

  def filter[I](f: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(i) if f(i) => emit(i)
      case _               => Halt()
    }.repeat

  def processFile[A, B](
    f: java.io.File,
    p: Process[String, A],
    z: B
  )(g: (B, A) => B): IO[B] = IO {

    @tailrec
    def go(ss: Iterator[String], cur: Process[String, A], acc: B): B =
      cur match {
        case Halt() => acc
        case Await(recv) =>
          val next = if (ss.hasNext) recv(Some(ss.next)) else recv(None)
          go(ss, next, acc)
        case Emit(h, t) => go(ss, t, g(acc, h))
      }
    val s = scala.io.Source.fromFile(f)
    try go(s.getLines, p, z)
    finally s.close
  }

  def count[I]: Process[I, Int] = {
    def go(n: Int): Process[I, Int] = await((i: I) => emit(n + 1, go(n + 1)))
    go(0)
  }

  def await[I, O](
    f: I => Process[I, O],
    fallback: Process[I, O] = Halt[I, O]()) = Await[I, O] {
    case Some(i) => f(i)
    case None    => fallback
  }

  def exists[I](f: I => Boolean): Process[I, Boolean] = lift(f) |> any

  def any[I]: Process[Boolean, Boolean] =
    loop(false)((b: Boolean, s) => (b || s, b || s))

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] = await(
    (i: I) =>
      f(i, z) match {
        case (o, s2) => emit(o, loop(s2)(f))
    }
  )
}
