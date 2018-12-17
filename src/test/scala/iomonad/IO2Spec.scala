package test.fpscala.iomonad

import org.scalatest.FlatSpec
import fpscala.iomonad.io2.IO

class IO2Spec extends FlatSpec {
  "test io2 forever" should "succeed" in {
    // IO.forever(IO.PrintLine("Still goinge")).run
    succeed
  }
}
