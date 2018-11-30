package fpscala.applicative

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing] {
  def apply[E](e: => E) = Failure(e, Vector.empty)
}

case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {
  def validationApplicative[E] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {
      def unit[A](a: => A): Validation[E, A] = Success(a)

      def map2[A, B, C](va: Validation[E, A], vb: Validation[E, B])(
        f: (A, B) => C): Validation[E, C] = (va, vb) match {
        case (Success(a), Success(b))           => Success(f(a, b))
        case (Success(a), b @ Failure(_, _))    => b
        case (a @ Failure(_, _), Success(b))    => a
        case (Failure(ha, ta), Failure(hb, tb)) => Failure(ha, ta ++ (hb +: tb))
      }

      def flatMap[A, B](va: Validation[E, A])(
        f: A => Validation[E, B]): Validation[E, B] = va match {
        case a @ Failure(_, _) => a
        case Success(a)        => f(a)
      }
    }
}

object Failure {
  def apply[E](e: => E): Failure[E] = Failure(e, Vector.empty)
}
