package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

final class Day09 extends FileLoader {

  def parseHistory(line: String): List[Int] = {
    line.split(" ").map(_.toInt).toList
  }

  @tailrec
  def calcHistDiffs(
      histDiffs: List[List[Int]]
  ): List[List[Int]] = {
    val lastDiff: List[Int] = histDiffs.head
    if (lastDiff.count(_ == 0) == lastDiff.size) {
      histDiffs
    } else {
      val newHist: List[Int] =
        lastDiff.sliding(2, 1).map(pair => pair(1) - pair(0)).toList
      calcHistDiffs(newHist :: histDiffs)
    }
  }

  @tailrec
  def calcNext(diff: Int, histDiffs: List[List[Int]]): Int = {
    val newDiff: Int = histDiffs.head.last + diff
    if (histDiffs.size == 1) {
      newDiff
    } else {
      calcNext(newDiff, histDiffs.tail)
    }
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val histories: List[List[Int]] = inputList.map(parseHistory)
    val nextValues: List[Int] =
      histories.map(history => {
        val diffs = calcHistDiffs(List(history))
        calcNext(0, diffs.tail)
      })

    val answer = nextValues.sum
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val histories: List[List[Int]] = inputList.map(parseHistory)
    val prevVaules: List[Int] =
      histories.map(history => {
        val diffs = calcHistDiffs(List(history.reverse))
        calcNext(0, diffs.tail)
      })

    val answer = prevVaules.sum
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
