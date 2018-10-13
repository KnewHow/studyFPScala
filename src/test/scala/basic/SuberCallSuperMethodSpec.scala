package test.fpscala.basic

import org.scalatest.FlatSpec
import fpscala.basic.Logger._

class SuberCallSuperMethodSpec extends FlatSpec {
  "test suberclass called super class method" should "success" in {
    SuberClass.fun1
    succeed
  }
}

sealed trait SuperClass {
  def fun1: Unit = Logger.info("super fun1")
  def fun2: Unit
}

object SuberClass extends SuperClass {
  // override def fun1: Unit = println("suber class fun1")
  override def fun1: Unit = super.fun1
  def fun2: Unit          = println("suber class fun2")
}
