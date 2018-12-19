package test.fpscala.iomonad

import org.scalatest.FlatSpec
import fpscala.iomonad.console.Console
import fpscala.iomonad.console.Console._
import fpscala.parallelism.NoBlockPar
import fpscala.parallelism.NoBlockPar.Par
import java.util.concurrent.Executors
import fpscala.iomonad.free.Free

class ConsoleIOSpec extends FlatSpec {
  val es = Executors.newFixedThreadPool(10)
  "test consoleParIO" should "succeed" in {
    // val p = Console.runConsolePar(Console.show)
    // val r = NoBlockPar.run(es)(p)
    // println(s"consoleParIO result ->$r")
    succeed
  }

  "test consoleThunkIO" should "succeed" in {
    // val p = Console.runConsoleThunk(Console.show)
    // val r = p()
    // println(s"consoleParIO result ->$r")
    succeed
  }

  "test consoleThunkIO" should "StackOverflow" in {
    // val p = Console.runConsole(
    // Free.FreeMonad.forever(Console.printlnIO("still going")))
    // val r = p()
    succeed
  }
}
