package fpscala.iomonad.console

import fpscala.parallelism.NoBlockPar.Par
import fpscala.parallelism.NoBlockPar
import fpscala.iomonad.free._
import fpscala.iomonad.free.Free._
import fpscala.iomonad.Monad

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
}

case object ReadLine extends Console[Option[String]] {

  def toPar   = NoBlockPar.delay(read)
  def toThunk = () => read
  def read: Option[String] =
    try {
      Some(readLine)
    } catch {
      case ex: Throwable => None
    }
}

case class PrintLine(line: String) extends Console[Unit] {
  def toPar   = NoBlockPar.lazyUnit(print)
  def toThunk = () => print
  def print   = println(line)

}

object Console {
  type ConsoleIO[A] = Free[Console, A]
  def readLineIO[A]: ConsoleIO[Option[String]] = Suspend(ReadLine)
  def printlnIO(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
  def show =
    for {
      _ <- printlnIO("show console io")
      r <- readLineIO
    } yield r

  val ConsoleToPar = new (Console ~> Par) {
    def apply[A](console: Console[A]): Par[A] = console.toPar
  }
  val ConsoleToThunk = new (Console ~> Function0) {
    def apply[A](console: Console[A]): Function0[A] = console.toThunk
  }

  implicit val ParMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A) = NoBlockPar.unit(a)
    def flatMap[A, B](fa: Par[A])(f: A => Par[B]) = NoBlockPar.fork {
      NoBlockPar.flatMap(fa)(f)
    }
  }

  implicit def Function0Monad: Monad[Function0] = new Monad[Function0] {
    def unit[A](a: => A): Function0[A]                        = () => a
    def flatMap[A, B](fa: Function0[A])(f: A => Function0[B]) = () => f(fa())()
  }

  def runConsolePar[A](cIO: ConsoleIO[A]): Par[A] = Free.run(cIO)(ConsoleToPar)
  def runConsoleThunk[A](cIO: ConsoleIO[A]): Function0[A] =
    Free.run(cIO)(ConsoleToThunk)

  def runConsole[A](cIO: ConsoleIO[A]): A = {
    Free.runTrampoline {
      Free.translate(cIO) {
        new (Console ~> Function0) {
          def apply[A](c: Console[A]) = c.toThunk
        }
      }
    }
  }
}
