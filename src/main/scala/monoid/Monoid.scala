package fpscala.monoid

import prop.gen._
import fpscala.basic.Logger.Logger
import java.util.concurrent._
import fpscala.parallelism.NoBlockPar
import fpscala.parallelism.NoBlockPar._

trait Monoid[A] {
  def op(a: A, b: A): A
  def zero: A
  def law(gen: Gen[A]): Prop = {
    Prop.forAll(
      for {
        x <- gen
        y <- gen
        z <- gen
      } yield (x, y, z)
    ) { g =>
      // Logger.info(s"g->$g")
      op(g._1, op(g._2, g._3)) == op(op(g._1, g._2), g._3) &&
      op(g._1, zero) == op(zero, g._1)
    }
  }
}

case object StringMoniod extends Monoid[String] {
  def op(a: String, b: String): String = a + b
  def zero                             = ""
}

case class ListMonoid[A]() extends Monoid[List[A]] {
  def op(a: List[A], b: List[A]): List[A] = a ++ b
  def zero                                = Nil
}

case object IntAddation extends Monoid[Int] {
  def op(a: Int, b: Int): Int = a + b
  def zero                    = 0
}

case object IntMultiplication extends Monoid[Int] {
  def op(a: Int, b: Int): Int = a * b
  def zero                    = 1
}

case object BooleanOr extends Monoid[Boolean] {
  def op(a: Boolean, b: Boolean) = a || b
  def zero                       = false
}

case object BooleanAnd extends Monoid[Boolean] {
  def op(a: Boolean, b: Boolean): Boolean = a && b
  def zero                                = true
}

case class OptionMonoid[A]() extends Monoid[Option[A]] {
  def op(a: Option[A], b: Option[A]): Option[A] = a orElse b
  def zero                                      = None
}

case class EndMonoid[A]() extends Monoid[A => A] {
  def op(a: (A => A), b: (A => A)): A => A = a andThen b
  def zero                                 = (a: A) => a

  def law(g: Gen[A], sg: Gen[A => A]): Prop = {
    val a = Gen.run(g).take(10).toList.head
    Prop.forAll(
      for {
        x <- sg
        y <- sg
        z <- sg
      } yield (x, y, z)
    ) { r =>
      op(r._1, op(r._2, r._3))(a) == op(op(r._1, r._2), r._3)(a) &&
      op(r._1, zero)(a) == op(zero, r._1)(a)
    }
  }

}
object Fold {
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, Monoid.dual(EndMonoid[B]()))(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, EndMonoid[B]())(a => b => f(b, a))(z)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    v.size match {
      case 0 =>
        m.zero
      case 1 =>
        m.op(m.zero, f(v.head))
      case _ =>
        val (l, r) = v.splitAt(v.size / 2)
        m.op(
          foldMapV(l, m)(f),
          foldMapV(r, m)(f)
        )
    }

  type Par[A] = (ExecutorService) => fpscala.parallelism.Future[A]
  def par[A](m: Monoid[A]) = new Monoid[Par[A]] {
    def op(a: Par[A], b: Par[A]) = NoBlockPar.map2(a, b)(m.op(_, _))
    def zero                     = NoBlockPar.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    NoBlockPar.parMap(v.toList)(f).flatMap { r =>
      foldMapV(r.toIndexedSeq, par(m))(b => NoBlockPar.lazyUnit(b))
    }

  def foldLeftViaParFoldMap[A, B](as: List[A], z: B)(f: (B, A) => B): Par[B] =
    parFoldMap(as.toIndexedSeq, EndMonoid[B]())(a => b => f(b, a)).map(r =>
      r(z))

  def foldRightViaParFoldMap[A, B](as: List[A], z: B)(f: (A, B) => B): Par[B] =
    parFoldMap(as.toIndexedSeq, Monoid.dual(EndMonoid[B]()))(f.curried).map(r =>
      r(z))

  def foldLeftViaParFoldMapLaw[A, B](gen: Gen[List[A]], z: Gen[B])(
    f: (B, A) => B): Prop =
    Prop.forAll(
      for {
        x <- gen
        y <- z
      } yield x -> y
    ) {
      case (as, z) =>
        val es = Executors.newFixedThreadPool(10)
        val pr = foldLeftViaParFoldMap(as, z)(f)
        as.foldLeft(z)(f) == pr.run(es)
    }

  def foldRightViaParFoldMapLaw[A, B](gen: Gen[List[A]], z: Gen[B])(
    f: (A, B) => B): Prop =
    Prop.forAll(
      for {
        x <- gen
        y <- z
      } yield x -> y
    ) {
      case (as, z) =>
        val es = Executors.newFixedThreadPool(10)
        as.foldRight(z)(f) == foldRightViaParFoldMap(as, z)(f).run(es)
    }

  def isOrderAsc(s: IndexedSeq[Int]): Boolean = {
    type Compare = Option[(Int, Int, Boolean)]
    val mon = new Monoid[Compare] {
      def op(a: Compare, b: Compare) = (a, b) match {
        case (Some((x1, y1, p)), Some((x2, y2, q))) =>
          Some(x1 min x2, y1 max y2, p && q && y1 <= x2)
        case (None, x) => x
        case (x, None) => x
      }
      def zero = None
    }
    foldMapV(s, mon)(i => Some(i, i, true)).map(_._3).getOrElse(true)
  }

  def foldRightViaFoldMapV[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMapV(as.toIndexedSeq, Monoid.dual(EndMonoid[B]()))(f.curried)(z)

  def foldLeftViaFoldMapV[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMapV(as.toIndexedSeq, EndMonoid[B]())(a => b => f(b, a))(z)

  def foldLeftViaFoldMapVLaw[A, B](gen: Gen[List[A]], z: Gen[B])(
    f: (B, A) => B): Prop =
    Prop.forAll(
      for {
        x <- gen
        y <- z
      } yield x -> y
    ) {
      case (as, z) =>
        as.foldLeft(z)(f) == foldLeftViaFoldMapV(as)(z)(f)
    }

  def foldRightViaFoldMapVLaw[A, B](gen: Gen[List[A]], z: Gen[B])(
    f: (A, B) => B): Prop =
    Prop.forAll(
      for {
        x <- gen
        y <- z
      } yield x -> y
    ) {
      case (as, z) =>
        as.foldRight(z)(f) == foldRightViaFoldMapV(as)(z)(f)
    }

  def foldLeftLaw[A, B](gen: Gen[List[A]], z: Gen[B])(f: (B, A) => B): Prop =
    Prop.forAll(
      for {
        x <- gen
        y <- z
      } yield x -> y
    ) {

      case (as, z) =>
        as.foldLeft(z)(f) == foldLeft(as)(z)(f)
    }

  def foldRightLaw[A, B](gen: Gen[List[A]], z: Gen[B])(f: (A, B) => B): Prop =
    Prop.forAll(
      for {
        x <- gen
        y <- z
      } yield x -> y
    ) {
      case (as, z) =>
        as.foldRight(z)(f) == foldRight(as)(z)(f)
    }
}

object Monoid {
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a: A, b: A): A = m.op(b, a)
    def zero              = m.zero
  }

  def product[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def zero = A.zero -> B.zero
      def op(x: (A, B), y: (A, B)): (A, B) =
        A.op(x._1, y._1) -> B.op(x._2, y._2)
    }
}
