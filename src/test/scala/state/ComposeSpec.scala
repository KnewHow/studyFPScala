package test.fpscala.state

import org.scalatest._

class ComposeSpec extends FlatSpec{
  def doubleNum: Int => Int = (x: Int) =>  x * 2

  def addOneNum: Int => Int = (x: Int) =>  x + 1

  def composeF1 = doubleNum.compose(addOneNum)

  "test composeF1 function" should "success" in {
    val r =  composeF1(2)
    assert(r == 6)
  }
}
