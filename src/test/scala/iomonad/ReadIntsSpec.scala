package test.fpscala.iomonad

import org.scalatest.FlatSpec
import fpscala.iomonad.io.IO

class ReadIntsSpec extends FlatSpec {
  "test readInts" should "succeed" in {
    // IO.readInts.flatMap(r => IO.PrintLine(r.toString)).run
    succeed
  }
}
