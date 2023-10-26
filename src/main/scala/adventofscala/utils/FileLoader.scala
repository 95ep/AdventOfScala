package adventofscala.utils

import scala.io.BufferedSource
import scala.io.Source

trait FileLoader {
  val inputPath: String
  def loadLines: Iterator[String] = {
    Source.fromResource(inputPath).getLines
  }
}
