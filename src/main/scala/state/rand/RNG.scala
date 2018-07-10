package fpscala.state

trait RNG {
  def nextInt: (Int, RNG)
  def nonNegativeInt(rng: RNG): (Int, RNG)
  def nonNegativeInt2(rng: RNG): (Int, RNG)
  def double(rng: RNG): (Double, RNG)
  def intDouble(rng: RNG): ((Int, Double), RNG)
  def doubleInt(rng: RNG): ((Double, Int), RNG)
  def double3(rng: RNG): ((Double, Double, Double), RNG)
  def ints(count: Int)(rng: RNG): (List[Int], RNG)
  type Rand[+A] = RNG => (A, RNG)
  def int: Rand[Int]
  def unit[A](a: A): Rand[A]
  def map[A, B](s: Rand[A])(f: A => B): Rand[B]
  def nonNegativeIntEvent: Rand[Int]
  def doubleViaMap: Rand[Double]
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C]
  def both[A, B](a: Rand[A], b: Rand[B]): Rand[(A, B)]

  def intDoubleViaBoth: Rand[(Int, Double)]

  def doubleIntViaBoth: Rand[(Double, Int)]

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]]

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]]

  def intsViaSequence(count: Int): Rand[List[Int]]

  def nonNegativeLessThan(n: Int): Rand[Int]

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B]

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B]

  def map2ViaFlatMap[A, B, C](as: Rand[A], bs: Rand[B])(f: (A, B) => C): Rand[C]

  def rollDie: Rand[Int]
}

case class SimpleRNG(val seed: Long) extends RNG {
  override def int: Rand[Int] = _.nextInt
  override def unit[A](a: A): Rand[A] = rng => (a, rng)
  override def map[A, B](s: Rand[A])(f: A => B): Rand[B] = { rng =>
    {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    {
      val (v, r1) = f(rng)
      g(v)(r1)
    }
  }

  override def map2[A, B, C](ra: Rand[A], rb: Rand[B])(
      f: (A, B) => C): Rand[C] = { rng =>
    {
      val (n1, r1) = ra(rng)
      val (n2, r2) = rb(r1)
      (f(n1, n2), r2)
    }
  }

  override def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((a, b) => map2(a, b)(_ :: _))

  override def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  override def both[A, B](a: Rand[A], b: Rand[B]): Rand[(A, B)] =
    map2(a, b)(_ -> _)

  override def nonNegativeIntEvent = map(nonNegativeInt)(i => i - i % 2)
  override def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble))

  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    n -> nextRNG
  }

  override def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    r match {
      case SimpleRNG(s) if (n > 0 && s > 0) => n -> r
      case _                                => nonNegativeInt(r)
    }
  }

  override def nonNegativeInt2(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  override def double(rng: RNG): (Double, RNG) = {
    val (v, r) = nonNegativeInt2(rng)
    (v / (Int.MaxValue.toDouble + 1), r)
  }

  override def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (v, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((v, d) -> r2)
  }

  override def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i) -> r)
  }
  override def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3) -> r3)
  }

  override def intDoubleViaBoth: Rand[(Int, Double)] = both(int, double)

  override def doubleIntViaBoth: Rand[(Double, Int)] = both(double, int)

  override def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    (count, rng) match {
      case (c, r) if c > 0 => {
        val (v1, r1) = r.nextInt
        ((v1 :: ints(c - 1)(r1)._1), r1)
      }
      case _ => (List.empty -> rng)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    {
      val (v, rng2) = nonNegativeInt(rng)
      val mod = v % n
      if (v + (n - 1) - mod >= 0) (mod, rng2) else nonNegativeLessThan(n)(rng2)
    }
  }

  def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) { rng =>
        (mod, rng)
      } else {
        nonNegativeLessThanViaFlatMap(n)
      }
    }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) {
    i => rng =>
      (f(i), rng)
  }

  def map2ViaFlatMap[A, B, C](as: Rand[A], bs: Rand[B])(
      f: (A, B) => C): Rand[C] = flatMap(as) { i => rng =>
    {
      val (v, r2) = bs(rng)
      (f(i, v), r2)
    }
  }

  def sequence2[A](as: List[Rand[A]]): Rand[List[A]] =
    as.foldRight(unit(List[A]()))((a, b) => map2(a, b)(_ :: _))

  def rollDie: Rand[Int] = mapViaFlatMap(nonNegativeLessThan(6))(_ + 1)

}
