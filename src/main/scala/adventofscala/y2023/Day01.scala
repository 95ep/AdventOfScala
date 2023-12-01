package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.util.matching.Regex

class Day01 extends FileLoader {

  def findPattern(pattern: Regex, s: String): List[(Int, String)] = {
    pattern.findAllMatchIn(s).map(m => (m.start, m.toString)).toList
  }

  def textToDigit(digit: String): String = {
    if (digit.length() > 1) {
      digit match {
        case "one"   => "1"
        case "two"   => "2"
        case "three" => "3"
        case "four"  => "4"
        case "five"  => "5"
        case "six"   => "6"
        case "seven" => "7"
        case "eight" => "8"
        case "nine"  => "9"
      }
    } else digit
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList

    val answer = inputList
      .map(l => findPattern("""\d""".r, l))
      .map(digitList => (digitList.head._2 + digitList.last._2).toInt)
      .sum

    println(s"Answer is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList

    val regexList = List(
      """\d""".r,
      """one""".r,
      """two""".r,
      """three""".r,
      """four""".r,
      """five""".r,
      """six""".r,
      """seven""".r,
      """eight""".r,
      """nine""".r
    )

    val answer = inputList
      .map(line =>
        regexList
          .flatMap(pattern =>
            findPattern(pattern, line).map((idx, m) => (idx, textToDigit(m)))
          )
          .sortBy(_._1)
      )
      .map(digitsForLine =>
        (digitsForLine.head._2 + digitsForLine.last._2).toInt
      )
      .sum

    println(s"Answer to part two is $answer")
    answer
  }
}
