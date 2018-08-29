package test.fpscala.parallelism

import org.scalatest._
import java.util.concurrent._

class FutureTaskSpec extends FlatSpec {
  def getFutureTask: FutureTask[String] = {
    new FutureTask[String](
      new Callable[String] {
        def call: String = {
          println(s"Callabel Thread Name->${Thread.currentThread.getName()}")
          Thread.sleep(5000)
          "lala"
        }
      }
    )
  }

  "Test Java Callabe" should "success" in {
    println(s"Test Thread Name->${Thread.currentThread.getName()}")
    val future = getFutureTask
    val thread = new Thread(future)
    thread.start
    val r = future.get
    println(s"result-> $r")
    assert(true)
  }
}
