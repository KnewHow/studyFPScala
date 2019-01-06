package fpscala.streamingio.io2

import fpscala.iomonad.io2.IO

sealed trait Process[F[_], O] {
  def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
    case Halt(e)          => Try(f(e))
    case Emit(h, t)       => Emit(h, t.onHalt(f))
    case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
  }

  def ++(p: Process[F, O]): Process[F, O] = this.onHalt {
    case End => p
    case err => Halt(err)
  }

  def await[F[_], A, O](
    req: F[A]
  )(recv: Either[Throwable, A] => Process[F, O]): Process[F, O] =
    Await(req, recv)

  def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = this match {
    case Halt(err)        => Halt(err)
    case Emit(o, t)       => Try(f(o)) ++ t.flatMap(f)
    case Await(req, recv) => await(req)(recv andThen (_.flatMap(f)))
  }

  def Try[F[_], O](p: => Process[F, O]): Process[F, O] =
    try p
    catch { case e: Throwable => Halt(e) }

  def onComplete(p: => Process[F, O]): Process[F, O] = this.onHalt {
    case End => p.asFinalizer
    case err => p.asFinalizer ++ Halt(err)
  }

  def asFinalizer: Process[F, O] = this match {
    case Halt(e)    => Halt(e)
    case Emit(h, t) => Emit(h, t.asFinalizer)
    case Await(req, recv) =>
      await(req) {
        case Left(Kill) => this.asFinalizer
        case x          => recv(x)
      }
  }

  def resource[R, O](acquire: IO[R])(
    use: R => Process[IO, O]
  )(release: R => Process[IO, O]): Process[IO, O] =
    await[IO, R, O](acquire) {
      case Right(r)  => use(r).onComplete(release(r))
      case Left(err) => Halt(err)
    }

  final def drain[O2]: Process[F, O2] = this match {
    case Halt(e)          => Halt(e)
    case Emit(h, t)       => t.drain
    case Await(req, recv) => Await(req, recv andThen (_.drain))
  }

  def eval[F[_], A](fa: F[A]): Process[F, A] = await(fa) {
    case Left(err) => Halt(err)
    case Right(a)  => Emit(a, Halt(End))
  }

  def eval_[F[_], A, B](fa: F[A]): Process[F, B] = eval(fa).drain[B]

}

case class Await[F[_], A, O](
  req: F[A],
  recv: Either[Throwable, A] => Process[F, O]
) extends Process[F, O]

case class Emit[F[_], O](
  head: O,
  tail: Process[F, O]
) extends Process[F, O]

case class Halt[F[_], O](err: Throwable) extends Process[F, O]

case object End extends Throwable

case object Kill extends Throwable

object Process {}
