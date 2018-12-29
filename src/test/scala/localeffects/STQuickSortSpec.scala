package test.fpscala.localeffects

import org.scalatest.FlatSpec
import fpscala.basic.Logger.Logger
import prop.gen._
import fpscala.localeffects._

class STQuickSortSpec extends FlatSpec {
  "test quick use ST" should "succeed" in {
    val a = List(5, 1, 2, 4, 3)
    val r = STArray.quickSort(a)
    assert(r == List(1, 2, 3, 4, 5))
  }

  "test quickSort" should "succeed" in {
    val g = Gen.listOfN(1000, Gen.choose(1, 10000))
    val p =
      sortedLaw(g)(a => STArray.quickSort(a.toList).toSeq)((a, b) => a <= b)
    assert(p.test())
  }

  /**
   * Whether the sequence is sorted
   */
  def sortedLaw[A](input: Gen[List[A]])(sortF: Seq[A] => Seq[A])(
    f: (A, A) => Boolean): Prop =
    Prop.forAll(input) { a =>
      val sorted = sortF(a)
      elementEquals(a, sorted.toList)(f) && isSorted[A](sorted)(f)
    }

  /**
   * Whether sorted List contains all elements in original List.
   * In this, `sorted` is sorted sequence,
   * so use binary search to make more effective.
   *
   * @param f Whether first element less than second element
   */
  private def elementEquals[A](original: List[A], sorted: List[A])(
    f: (A, A) => Boolean): Boolean = {
    original.size == sorted.size && original.forall { r =>
      // Search.binarySearch(sorted, r)(f) != -1
      // it is more effective than binary search
      sorted.contains(r)
    }
  }

  private def isSorted[A](sorted: Seq[A])(f: (A, A) => Boolean): Boolean = {
    var i = 0
    while (i < sorted.size - 1) {
      if (!f(sorted(i), sorted(i + 1))) {
        return false
      }
      i += 1
    }
    true
  }
}
