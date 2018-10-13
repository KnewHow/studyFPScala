package test.fpscala.parallelism

import org.scalatest._
import java.util.concurrent._

class JavaFutureSpec extends FlatSpec {
  def getTask: Callable[String] = {
    new Callable[String] {
      def call: String = {
        println(s"Callabel Thread Name->${Thread.currentThread.getName()}")
        Thread.sleep(5000)
        "lala"
      }
    }
  }
  "Test Java Future" should "success" in {
    val ex     = Executors.newFixedThreadPool(10)
    val task   = getTask
    val future = ex.submit(task)
    val r      = future.get()
    println(s"Test Thread Name->${Thread.currentThread.getName()}")
    println(s"result->$r")
    assert(r == "lala")
  }
}
