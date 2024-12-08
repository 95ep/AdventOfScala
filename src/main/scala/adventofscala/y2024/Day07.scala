package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day07 extends FileLoader {
  def parseLine(line: String): (Long, List[Long]) = {
    val parts = line.split(":")
    val sum = parts.head.toLong
    val terms = parts(1).strip().split(" ").map(_.toLong).toList
    (sum, terms)
  }

  @tailrec
  private def allPossibleSums(
      terms: List[Long],
      allSums: Set[Long],
      part2: Boolean
  ): Set[Long] = {
    if (terms.isEmpty) allSums
    else {
      val nextTerm = terms.head
      val newSums = allSums.flatMap(s => {
        if (part2) {
          List(
            s + nextTerm,
            s * nextTerm,
            (s.toString() + nextTerm.toString()).toLong
          )
        } else { List(s + nextTerm, s * nextTerm) }
      })
      allPossibleSums(terms.tail, newSums, part2)
    }
  }

  def part1(inputPath: String): Long = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val equations = inputList.map(parseLine)
    val validEquations = equations.filter((rightSum, terms) =>
      allPossibleSums(terms, Set(0), false).contains(rightSum)
    )

    val answer = validEquations.map(_._1).sum
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Long = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val equations = inputList.map(parseLine)
    val validEquations = equations.filter((rightSum, terms) =>
      allPossibleSums(terms, Set(0), true).contains(rightSum)
    )

    val answer = validEquations.map(_._1).sum
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
