package fpscala.localeffects

import scala.reflect.Manifest

sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)
  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S, A](a: => A): ST[S, A] = new ST[S, A] {
    def run(s: S) = {
      lazy val cash = a
      (cash, s)
    }
  }
  def run[A](r: RunnableST[A]): A = r.apply[Unit].run(())._1
}

sealed trait STRef[S, A] {
  protected var cell: A
  def read: ST[S, A] = ST(cell)
  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      cell = a
      () -> s
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(
    new STRef[S, A] {
      var cell: A = a
    }
  )
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

sealed abstract class STArray[S, A](implicit manifect: Manifest[A]) {
  protected def value: Array[A]
  def size                   = ST(value.size)
  def read(i: Int): ST[S, A] = ST(value(i))
  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      value(i) = a
      () -> s
    }
  }
  def freeze: ST[S, List[A]] = ST(value.toList)

  def fill(xs: Map[Int, A]): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      val fut = xs.foreach {
        case (k, v) => write(k, v)
      }
      fut -> s
    }
  }

  def swap(i: Int, j: Int): ST[S, Unit] =
    for {
      v1 <- read(i)
      v2 <- read(j)
      _  <- write(i, v2)
      _  <- write(j, v2)
    } yield ()

  def noop[S] = ST[S, Unit](())

  def partition(
    a: STArray[S, Int],
    begin: Int,
    end: Int,
    pivot: Int): ST[S, Int] =
    for {
      pv <- a.read(pivot)
      _  <- swap(end, pivot)
      j  <- STRef(begin)
      _ <- (begin until end).foldLeft(noop[S]) { (s, i) =>
        for {
          _  <- s
          vi <- a.read(i)
          _ <- if (vi < pv) (for {
            jv <- j.read
            _  <- swap(jv, i)
            _  <- j.write(jv + 1)
          } yield ())
          else noop[S]
        } yield ()
      }
      jv <- j.read
      _  <- swap(jv, end)
    } yield jv

  def qs(a: STArray[S, Int], begin: Int, end: Int): ST[S, Unit] =
    if (begin < end) (for {
      q <- partition(a, begin, end, (begin + end) / 2)
      _ <- qs(a, begin, q - 1)
      _ <- qs(a, q + 1, end)
    } yield ())
    else noop[S]
}

object STArray {
  def apply[S, A: Manifest](sz: Int, a: A): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      def value = Array.fill(sz)(a)
    })

  def apply[S, A: Manifest](a: Array[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      def value = a
    })
}
