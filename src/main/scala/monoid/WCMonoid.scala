package fpscala.monoid

import fpscala.basic.Logger.Logger

sealed trait WC

case class Stub(chars: String) extends WC

case class Part(lStub: String, words: Int, rStub: String) extends WC

case object WCMonoid extends Monoid[WC] {
  def zero = Stub("")
  def op(a: WC, b: WC): WC = (a, b) match {
    case (Stub(c), Stub(d))       => Stub(c + d)
    case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
    case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
    case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
      Part(l1, w1 + (if ((r1 + l2).nonEmpty) 1 else 0) + w2, r2)
  }
}

object WC {
  def count(s: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)

    def unStub(s: String) = s.length min 1

    Fold.foldMapV(s.toIndexedSeq, WCMonoid)(wc) match {
      case Stub(s)       => unStub(s)
      case Part(l, n, r) => unStub(l) + n + unStub(r)
    }
  }

}
