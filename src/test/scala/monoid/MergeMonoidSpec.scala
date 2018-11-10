package test.fpscala.monoid

import fpscala.monoid._
import org.scalatest.FlatSpec

class MergeMonoidSpec extends FlatSpec {

  "test mapMergeMonoid" should "succeed" in {
    val M: Monoid[Map[String, Map[String, Int]]] =
      MergeMonoid.mapMergeMonoid(MergeMonoid.mapMergeMonoid(IntAddation))

    val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
    val m2 = Map("o1" -> Map("i2" -> 3))

    val r1 = M.op(m1, m2)
    val r2 = Map("o1" -> Map("i1" -> 1, "i2" -> 5))
    assert(r1 == r2)
  }

  "test calculate element appear times" should "succeed" in {
    def bag[A](as: IndexedSeq[A]): Map[A, Int] =
      Fold.foldMapV(as, MergeMonoid.mapMergeMonoid[A, Int](IntAddation))(a =>
        Map[A, Int](a -> 1))

    val a = IndexedSeq("a", "b", "a")
    val r = bag(a)
    assert(r == Map("a" -> 2, "b" -> 1))
  }

  "test product monoid" should "succeed" in {
    val a = IndexedSeq(1, 2, 3, 4)
    val m = Monoid.product(IntAddation, IntAddation)
    val r: (Int, Int) =
      ListFoldable.foldMap[Int, (Int, Int)](a.toList)(a => (1, a))(m)
    val mAverage = r._2 / r._1
    val average  = a.foldLeft(0)(_ + _) / a.size
    assert(mAverage == average)
  }
}
