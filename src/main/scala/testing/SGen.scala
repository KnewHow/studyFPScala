package fpscala.testing

case class SGen[A](forSize: Int => Gen[A]) {
  def unit[A](a: => A): SGen[A] = SGen(forSize(_).unit(a))

  def boolean: SGen[Boolean] = SGen(forSize(_).boolean)

  def listOfN(n: Int, g: SGen[A]): SGen[List[A]] = {
    SGen(r => forSize(r).listOfN(n, g.forSize(r)))
  }

}
