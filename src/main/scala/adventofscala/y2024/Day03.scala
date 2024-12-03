package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.util.matching.Regex

class Day03 extends FileLoader {

  def findMul(s: String): Iterator[Regex.Match] = {

    val pattern = raw"mul\((\d{1,3}),(\d{1,3})\)".r
    pattern.findAllMatchIn(s)
  }
  def findDo(s: String): Iterator[Regex.Match] = {
    val pattern = raw"do\(\)".r
    pattern.findAllMatchIn(s)
  }

  def findDont(s: String): Iterator[Regex.Match] = {
    val pattern = raw"don't\(\)".r
    pattern.findAllMatchIn(s)
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val matches = inputList.flatMap(l => findMul(l))
    val products = matches.map(m => {
      val x = m.group(1).toInt
      val y = m.group(2).toInt
      x * y
    })

    val answer = products.sum
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def isEnabled(
      idx: Int,
      doList: List[Regex.Match],
      dontList: List[Regex.Match]
  ) = {
    val lastDoIdx =
      doList.map(_.start).filter(_ < idx).sorted.lastOption.getOrElse(0)
    val lastDontIdx =
      dontList.map(_.start).filter(_ < idx).sorted.lastOption.getOrElse(-1)

    lastDoIdx > lastDontIdx

  }
  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val concatString = inputList.mkString
    val muls = findMul(concatString)
    val _do = findDo(concatString).toList
    val dont = findDont(concatString).toList

    val products = for m <- muls if isEnabled(m.start, _do, dont)
    yield m.group(1).toInt * m.group(2).toInt
    val answer = products.sum
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
