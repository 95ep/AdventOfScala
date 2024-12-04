package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.compiletime.ops.int

class Day04 extends FileLoader {

  def getDown(x: Int, y: Int, input: List[String], n: Int) = {
    for iy <- y until y + n if iy < input.length
    yield input(x)(iy)
  }

  def getRight(x: Int, y: Int, input: List[String], n: Int) = {
    for ix <- x until x + n if ix < input.length
    yield input(ix)(y)
  }

  def getDiag1(x: Int, y: Int, input: List[String], n: Int) = {
    for i <- 0 until n if x + i < input.length && y + i < input.length
    yield input(x + i)(y + i)
  }

  def getDiag2(x: Int, y: Int, input: List[String], n: Int) = {
    for i <- 0 until n if x + i < input.length && y - i > -1
    yield input(x + i)(y - i)
  }

  def findNXmas(x: Int, y: Int, input: List[String]) = {
    val down = getDown(x, y, input, 4)
    val right = getRight(x, y, input, 4)
    val diag1 = getDiag1(x, y, input, 4)
    val diag2 = getDiag2(x, y, input, 4)

    List(down, right, diag1, diag2)
      .map(_.mkString)
      .count(s => s == "XMAS" || s == "SAMX")
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList

    val answer = (for
      x <- 0 until inputList.size
      y <- 0 until inputList.size
    yield findNXmas(x, y, inputList)).sum

    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def matchesMas(s: String): Boolean = s == "MAS" || s == "SAM"

  def findMas(x: Int, y: Int, input: List[String]) = {
    val diag1 =
      Option.when(matchesMas(getDiag1(x, y, input, 3).mkString))(x + 1, y + 1)
    val diag2 =
      Option.when(matchesMas(getDiag2(x, y, input, 3).mkString))(x + 1, y - 1)

    List(diag1, diag2).flatten
  }
  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList

    val mas = (for
      x <- 0 until inputList.size
      y <- 0 until inputList.size
    yield findMas(x, y, inputList)).flatten

    val answer =
      mas.groupBy(identity).mapValues(_.size).filter((_, n) => n > 1).size
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
