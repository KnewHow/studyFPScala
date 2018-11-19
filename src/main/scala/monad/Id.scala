package fpscala.monad

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B]         = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  val IdMonod = new Monad[Id] {
    def unit[A](a: => A)                               = Id(a)
    def flatMap[A, B](id: Id[A])(f: A => Id[B]): Id[B] = f(id.value)
  }
}
