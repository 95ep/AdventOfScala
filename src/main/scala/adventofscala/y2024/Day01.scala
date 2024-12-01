package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.compiletime.ops.int

class Day01 extends FileLoader {

  def extractList(in: List[String], idx: Int): List[Int] = {
    in.map(_.split(" ")).map(_(idx)).map(_.toInt).sorted
  }
  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val l1 = extractList(inputList, 0)
    val l2 = extractList(inputList, 3)

    val diffs =
      for (i1, i2) <- (l1 zip l2)
      yield (i1 - i2).abs

    val answer = diffs.sum
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val l1 = extractList(inputList, 0)
    val l2 = extractList(inputList, 3)

    val similarities =
      for i <- l1
      yield i * l2.filter((p) => p == i).length

    val answer = similarities.sum
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
