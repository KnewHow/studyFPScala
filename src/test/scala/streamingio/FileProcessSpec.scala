package test.fpscala.streamingio

import org.scalatest.FlatSpec
import fpscala.streamingio.Process._
import fpscala.streamingio._
import java.io.File
import fpscala.basic.Logger.Logger
import scala.io.Source

class FileProcessSpec extends FlatSpec {
  "test statis lines" should "succeed" in {
    val f    = new File("resource/test.txt")
    val line = processFile(f, count |> exists(_ < 4000), false)(_ || _)
    assert(line.run)
  }

  "test read and convert" should "success" in {
    def toCeksius(fahren: Double): Double = (5.0 / 9.0) * (fahren - 32.0)
    val f                                 = new File("resource/fahrenheit.txt")
    val conver: Process[String, Double] =
      filter((line: String) => !line.startsWith("#")) |>
        filter((line: String) => line.trim.nonEmpty) |>
        lift((line: String) => (toCeksius(line.toDouble)))
    val r = processFile(f, conver, List[Double]())((b, a) => a :: b)
    assert(r.run == List[Double](15, 25))
  }
}
