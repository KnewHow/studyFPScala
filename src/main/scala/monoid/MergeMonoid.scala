package fpscala.monoid

object MergeMonoid {
  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(
            k,
            V.op(
              a.getOrElse(k, V.zero),
              b.getOrElse(k, V.zero)
            ))
        }
    }

  def functionMonoid[A, B](m: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def zero                                 = (a: A) => m.zero
    def op(f: (A => B), g: (A => B)): A => B = (a: A) => m.op(f(a), g(a))
  }
}
