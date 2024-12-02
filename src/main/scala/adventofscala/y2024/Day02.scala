package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day02 extends FileLoader {

  def toReports(inputList: List[String]): List[List[Int]] =
    inputList.map(_.split(" ").map(_.toInt).toList)

  @tailrec
  private def checkIncreasing(report: List[Int]): Boolean = {
    if (report.tail.length == 0) {
      true
    } else { report.head < report.tail.head && checkIncreasing(report.tail) }
  }

  @tailrec
  private def checkDiff(report: List[Int]): Boolean = {
    if (report.tail.length == 0) {
      true
    } else {
      val diff = (report.head - report.tail.head).abs
      diff < 4 && diff > 0 && checkDiff(report.tail)
    }
  }

  def checkSafe(report: List[Int]): Boolean = {
    (checkIncreasing(report) || checkIncreasing(report.reverse)) && checkDiff(
      report
    )
  }

  def checkSafeWithDampener(report: List[Int]) = {
    val isSafe = checkSafe(report)
    if (isSafe) {
      true
    } else {
      val allChecks =
        for i <- 0 to report.length - 1
        yield checkSafe(report.take(i) ::: report.drop(i + 1))
      allChecks.exists(p => p)
    }

  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val reports = toReports(inputList)

    val answer = reports.map(checkSafe(_)).count(b => b)
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val reports = toReports(inputList)
    val answer = reports.map(checkSafeWithDampener(_)).count(b => b)
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
