package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day12 extends FileLoader {

  def parseLine(line: String, repeats: Int): (String, List[Int], List[Int]) = {
    val springs = ("?" + line.split(" ")(0)).repeat(repeats).drop(1)
    val idxUnknown = springs.zipWithIndex
      .filter((spring, _) => spring == '?')
      .map((_, idx) => idx)
      .toList

    val groups =
      Range(0, repeats)
        .flatMap(_ => line.split(" ")(1).split(',').map(_.toInt))
        .toList
    (springs, idxUnknown, groups)
  }

  def validPlacement(
      springs: String,
      startIdx: Int,
      groupSize: Int
  ): Boolean = {
    val endIdx = startIdx + groupSize
    if (endIdx > springs.size) false
    else if (endIdx == springs.size) {
      val r = s"""[#?]{$groupSize}""".r
      r.matches(springs.substring(startIdx, endIdx))
    } else {
      val r = s"""[#?]{$groupSize}[.?]""".r
      r.matches(springs.substring(startIdx, endIdx + 1))
    }
  }

  @tailrec
  private def possibleArrangements(
      springs: String,
      groups: List[Int],
      prevPlacement: Map[Int, Long]
  ): Long = {
    if (groups.isEmpty)
      prevPlacement
        .filter((k, _) => springs.drop(k).indexOf('#') == -1)
        .foldLeft(0L)(_ + _._2)
    else {
      val newPrevPlacement = prevPlacement
        .map((prev, count) => {
          val lastStart =
            if (springs.drop(prev).indexOf("#") != -1)
              springs.drop(prev).indexOf("#") + (if (prev != -1) prev else 0)
            else springs.size - 1
          Range(prev + 1, lastStart + 1)
            .filter(i => validPlacement(springs, i, groups.head))
            .map(i => (i + groups.head, count))
            .groupMapReduce(_._1)(_._2)((a, b) => a + b)
        })
        .reduce((m1, m2) =>
          m1 ++ m2.map { case (k, v) => k -> (v + m1.getOrElse(k, 0L)) }
        )

      possibleArrangements(springs, groups.tail, newPrevPlacement)
    }
  }

  def solution2(inputList: List[String], repeats: Int): Long = {
    println("Solution 2")
    inputList
      .map(line => {
        val parsedLine = parseLine(line, repeats)
        possibleArrangements(parsedLine._1, parsedLine._3, Map(-1 -> 1L))
      })
      .sum
  }

  def part1(inputPath: String): Long = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val answer = solution2(inputList, 1)

    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Long = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val answer = solution2(inputList, 5)

    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
