package fpscala.localeffects

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
