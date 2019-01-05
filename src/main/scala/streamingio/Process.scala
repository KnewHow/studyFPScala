package fpscala.streamingio

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

  def liftOne(f: I => O): Process[I, O] = Await {
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

  def lift(f: I => O) = liftOne(f).repeat

  def emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()): Process[I, O] =
    Emit(head, tail)

  def filter(f: I => Boolean): Process[I, I] =
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

  def take(n: Int): Process[I, I] =
    if (n <= 0)
      Halt()
    else
      await(i => emit(i, take(n - 1)))

  def drop(n: Int): Process[I, I] =
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
  def sumViaLoop                    = loop(0.0)((i: Double, s) => (i + s, i + s))
  def countViaLoop: Process[I, Int] = loop(0)((i: I, c) => (c + 1, c + 1))
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
}
