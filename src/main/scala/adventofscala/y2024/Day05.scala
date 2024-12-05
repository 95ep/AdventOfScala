package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day05 extends FileLoader {

  def loadRules(inputLines: List[String]): List[String] = {
    val pattern = raw"\d{2}\|\d{2}.*".r
    inputLines.filter(l => pattern.matches(l))
  }

  def loadUpdates(inputLines: List[String]): List[List[String]] = {
    val pattern = raw"(\d{2},).*".r
    inputLines.filter(l => pattern.matches(l)).map(l => l.split(",").toList)
  }

  def checkUpdateValid(
      update: List[String],
      rules: List[String]
  ): Boolean = {
    val l =
      for i <- 0 until update.size - 1
      yield rules.contains(
        update(i + 1) + "|" + update(i)
      )

    l.count(b => b) == 0
  }

  def getMiddle(update: List[String]) = update(update.size / 2).toInt
  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val rules = loadRules(inputList)
    val updates = loadUpdates(inputList)

    val answer =
      updates.filter(u => checkUpdateValid(u, rules)).map(u => getMiddle(u)).sum
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  @tailrec
  final def fixOrder(
      update: List[String],
      rules: List[String]
  ): List[String] = {
    if (checkUpdateValid(update, rules)) update
    else {
      val badIdx =
        for i <- 0 until update.size - 1
        yield
          if (
            rules.contains(
              update(i + 1) + "|" + update(i)
            )
          ) i
          else -1
      val i = badIdx.filter(i => i > -1).head
      val newUpdate =
        update.take(i) ::: update(i + 1) :: update(i) :: update.takeRight(
          update.size - (i + 2)
        )
      fixOrder(newUpdate, rules)
    }

  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val rules = loadRules(inputList)
    val updates = loadUpdates(inputList)
    val answer = updates
      .filter(u => !checkUpdateValid(u, rules))
      .map(u => getMiddle(fixOrder(u, rules)))
      .sum

    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
