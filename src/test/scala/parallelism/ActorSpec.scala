package test.fpscala.parallelism

import fpscala.parallelism._
import java.util.concurrent._
import org.scalatest._

class ActorSpec extends FlatSpec {
  val S = Executors.newFixedThreadPool(10)
  val echoer = Actor[String](S) { msg =>
    println(s"Got message -> $msg")
  }

  "test actor" should "success" in {
    echoer ! "123"
    echoer ! "456"
    println("test finish")
    assert(true)
  }
}
