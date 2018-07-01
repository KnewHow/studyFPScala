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
}

case class SimpleRNG(val seed: Long) extends RNG {
  override def int: Rand[Int] = _.nextInt
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    n -> nextRNG
  }

  override def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    r match {
      case SimpleRNG(s) if (n >0 && s > 0) => n -> r
      case _ => nonNegativeInt(r)
    }
  }

  override def nonNegativeInt2(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  override def double(rng: RNG): (Double, RNG) = {
    val (v, r) = nonNegativeInt2(rng)
    ( v / (Int.MaxValue.toDouble + 1), r)
  }

  override def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (v, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((v, d) -> r2)
  }

  override def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r)  = intDouble(rng)
    ((d, i) -> r)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3) -> r3)
  }

  override def ints(count: Int)(rng: RNG): (List[Int], RNG) = (count, rng) match {
    case (c, r) if c > 0 => {
      val (v1, r1) = r.nextInt
      ((v1 :: ints(c-1)(r1)._1), r1)
    }
    case _  => (List.empty -> rng)
  }

}
