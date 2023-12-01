package adventofscala.utils

import scala.io.BufferedSource
import scala.io.Source

trait FileLoader {
  def loadLines(inputPath: String): Iterator[String] = {
    Source.fromResource(inputPath).getLines
  }
}
