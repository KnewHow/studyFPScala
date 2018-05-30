package test.fpscala.datastructures

import org.scalatest._

import fpscala.datastructures._

class FoldLeftAndRightInteroperateSpec extends FlatSpec {
  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
  List.foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
}
