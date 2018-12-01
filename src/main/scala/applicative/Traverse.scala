package fpscala.applicative

trait Traverse[F[_]] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]: Applicative, A, B](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Traverse {
  val listTraverse = new Traverse[List] {
    def map[A, B](fa: List[A])(f: A => B) = fa.map(f)
  }
  val optionTraverse = new Traverse[Option] {
    def map[A, B](fa: Option[A])(f: A => B) = fa map (f)
  }
}
