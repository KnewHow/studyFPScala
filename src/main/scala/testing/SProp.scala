package fpscala.testing

import fpscala.laziness.Stream

case class SProp(run: (Int, Int, RNG) => Result) {
  def check(
      maxSize: Int = 100,
      testCases: Int = 100,
      rng: RNG = RNG(System.currentTimeMillis)
  ): Unit = this.run(maxSize, testCases, rng) match {
    case Passed => println("all test cases passed")
    case Falsified(msg, sc) =>
      println(s"test case failure, case by $msg, But success $sc times")
  }

  def &&(sp: SProp): SProp = SProp { (maxSize, testCases, rng) =>
    (this.run(maxSize, testCases, rng), sp.run(maxSize, testCases, rng)) match {
      case (Passed, Passed)       => Passed
      case (f: Falsified, Passed) => f
      case (Passed, f: Falsified) => f
      case (f1: Falsified, f2: Falsified) =>
        Falsified(f1.failure + "," + f2.failure, f1.successes + f2.successes)
    }
  }

  def ||(sp: SProp): SProp = SProp { (maxSize, testCases, rng) =>
    (this.run(maxSize, testCases, rng), sp.run(maxSize, testCases, rng)) match {
      case (Falsified(f1, s1), Falsified(f2, s2)) =>
        Falsified(f1 + "," + f2, s1 + s2)
      case _ => Passed
    }
  }
}

object SProp {
  def forAll[A](sg: SGen[A])(f: A => Boolean): SProp = forAll(sg.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): SProp = SProp {
    (maxSize, testCases, rng) =>
      {
        val casesPerSize = (testCases + (maxSize - 1) / maxSize)
        val props: Stream[Prop] = Stream
          .from(0)
          .take((maxSize min casesPerSize) + 1)
          .map(i => Prop.forAll(g(i))(f))
        val sp: SProp = props
          .map(p =>
            SProp { (maxSize, casesPerSize, rng) =>
              p.run(maxSize, rng)
          })
          .toList
          .reduce(_ && _)
        sp.run(maxSize, testCases, rng)
      }
  }
}
