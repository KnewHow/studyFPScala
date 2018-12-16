package fpscala.iomonad

import scala.io.StdIn

sealed trait IO[A] { self =>
  def run: A
  def map[B](f: A => B): IO[B]              = new IO[B] { def run = f(self.run) }
  def flatMap[B](f: A => IO[B]): IO[B]      = new IO[B] { def run = f(self.run).run }
  def apply(a: => A): IO[A]                 = new IO[A] { def run = a }
  def map2[B, C](io: IO[B])(f: (A, B) => C) = IO.map2(self, io)(f)
  def product[B](io: IO[B]): IO[(A, B)]     = IO.product(self, io)
  def **[B](io: IO[B]): IO[(A, B)]          = self.product(io)
}

sealed class IORef[A](var value: A) {
  def set(a: A): IO[A] = IO { value = a; a }
  def get: IO[A]       = IO { value }
  def modify(f: A => A): IO[A] = get flatMap { a =>
    set(f(a))
  }
}

object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] { def run = a }

  def flatMap[A, B](io: IO[A])(f: A => IO[B]): IO[B] = new IO[B] {
    def run = f(io.run).run
  }

  def apply[A](a: => A): IO[A] = unit(a)

  def ReadLine: IO[String] = IO(readLine)

  def PrintLine(msg: String): IO[Unit] = IO(println(msg))

  def echo: IO[Unit] = ReadLine.flatMap(PrintLine)

  def readInt: IO[Int] = ReadLine.map(_.toInt)

  def readInts: IO[(Int, Int)] = readInt ** readInt

  def lines: IO[List[String]] = replicate(10)(ReadLine)

  def ref[A](a: A): IO[IORef[A]] = IO { new IORef(a) }

  def relicateM(n: Int) =
    for {
      _ <- PrintLine("please input fahren")
      d <- ReadLine.map(_.toDouble)
      _ <- PrintLine(
        s"converte celsius result is ${TemperatureConverte.fahrenheitToCelsius(d)}")
    } yield ()

  def factorial(n: Int): IO[Int] =
    for {
      acc <- ref(1)
      _   <- foreachM(1 to n toStream)(i => acc.modify(_ * i).skip)
      r   <- acc.get
    } yield r

  val helpstring = """
  | The Amazing Factorial REPL, v2.0
  | q - quit
  | <number> - compute the factorial of the given number
  | <anything else> - bomb with horrible error
  """.trim.stripMargin

  def factorialREPL: IO[Unit] = sequence_(
    PrintLine(helpstring),
    doWhile(ReadLine) { line =>
      when(line != "q") {
        for {
          r <- factorial(line.toInt)
          _ <- PrintLine(r.toString)
        } yield ()
      }
    }
  )
}

object TemperatureConverte {
  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0
}
