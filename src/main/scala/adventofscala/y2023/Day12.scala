package adventofscala.y2023
import adventofscala.utils.FileLoader

class Day12 extends FileLoader {

  def parseLine(line: String): (String, List[Int], List[Int]) = {
    val springs = (line.split(" ")(0))
    val idxUnknown = springs.zipWithIndex
      .filter((spring, _) => spring == '?')
      .map((_, idx) => idx)
      .toList

    val groups = line.split(" ")(1).split(',').map(_.toInt).toList
    (springs, idxUnknown, groups)
  }

  def isValid(
      arrangement: List[Int],
      row: String,
      groups: List[Int]
  ): Boolean = {
    val updatedRow = arrangement.foldLeft(row)((s, i) => s.updated(i, '#'))
    val groupSizes = """#+""".r.findAllIn(updatedRow).map(_.size).toList
    groupSizes == groups
  }

  def solution(inputList: List[String]): Int = {
    inputList
      .map(line => {
        val parsedLine = parseLine(line)
        val damagedToPlace = parsedLine._3.sum - parsedLine._1.count(_ == '#')
        val possibleArrangements = parsedLine._2.combinations(damagedToPlace)
        val validArrangements = possibleArrangements.filter(arrangement =>
          isValid(arrangement, parsedLine._1, parsedLine._3)
        )
        validArrangements.size
      })
      .sum
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val answer = solution(inputList)

    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    // val answer = solution(inputList, 5)
    val answer = 1

    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
