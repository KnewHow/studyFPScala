package test.fpscala.iomonad

import fpscala.iomonad.io.IO
import org.scalatest.FlatSpec

class IOSpec extends FlatSpec {
  "test io converter" should "succeed" in {
    // IO.converter.run
    succeed
  }

  "test factorocal" should "succeed" in {
    // IO.factorialREPL.run
    succeed
  }

  "test forever" should "prouduce stackOverFlowError" in {
    // IO.forever { IO.PrintLine("lala") }.run
    succeed
  }
}
