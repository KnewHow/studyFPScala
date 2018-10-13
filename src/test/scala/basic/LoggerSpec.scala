package test.fpscala.basic

import fpscala.basic.Logger.Logger
import org.scalatest.FlatSpec
class LoggerSpec extends FlatSpec {
  "choose which logger is more elegant" should "success" in {
    Logger.info(s"lala")
  }
}
