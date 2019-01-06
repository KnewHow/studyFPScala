package test.fpscala.streamingio

import org.scalatest.FlatSpec
import fpscala.streamingio.Process._
import java.io.File
import fpscala.basic.Logger.Logger
import scala.io.Source

class FileProcessSpec extends FlatSpec {
  "test read lines" should "succeed" in {
    val f = new File("resource/test.txt")
    // Logger.info(s"${f.getAbsolutePath}")
    val line = processFile(f, count |> exists(_ < 4000), false)(_ || _)
    assert(line.run)
  }
}
