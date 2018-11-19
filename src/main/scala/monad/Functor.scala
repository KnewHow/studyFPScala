package fpscala.monad

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fa: F[(A, B)]): (F[A], F[B]) =
    map(fa)(_._1) -> map(fa)(_._2)

  def codistribute[A, B](fa: Either[F[A], F[B]]): F[Either[A, B]] = fa match {
    case Left(f)  => map(f)(Left(_))
    case Right(f) => map(f)(Right(_))
  }
}

object ListFunctor extends Functor[List] {
  def map[A, B](as: List[A])(f: A => B): List[B] = as map (f)
}
