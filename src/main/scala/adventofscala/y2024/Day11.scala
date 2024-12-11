package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day11 extends FileLoader {

  case class Stone(number: Long, copies: Long) {
    def splitStone: List[Stone] = {
      if (number == 0) List(Stone(1, copies))
      else if (number.toString().length() % 2 == 0) {
        val numberAsString = number.toString()
        val nDigits = numberAsString.length()
        List(
          Stone(numberAsString.take(nDigits / 2).toLong, copies),
          Stone(numberAsString.takeRight(nDigits / 2).toLong, copies)
        )
      } else { List(Stone(number * 2024, copies)) }
    }
  }

  @tailrec
  final def splitStones(
      stones: List[Stone],
      idx: Int,
      max: Int
  ): List[Stone] = {
    if (idx == max) stones
    else {
      val splittedStones = stones.toList.flatMap(_.splitStone)
      val copiesPerNumber =
        splittedStones.groupBy(_.number).map((k, v) => k -> v.map(_.copies).sum)
      val newStones = copiesPerNumber.map((k, v) => Stone(k, v)).toList
      splitStones(newStones, idx + 1, max)
    }
  }

  def part1(inputPath: String): Long = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val stones = inputList.head.split(" ").map(s => Stone(s.toInt, 1)).toList

    val blinkedStones = splitStones(stones, 0, 25)
    val answer = blinkedStones.map(_.copies).sum
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Long = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val stones = inputList.head.split(" ").map(s => Stone(s.toInt, 1)).toList

    val blinkedStones = splitStones(stones, 0, 75)
    val answer = blinkedStones.map(_.copies).sum
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }
}
