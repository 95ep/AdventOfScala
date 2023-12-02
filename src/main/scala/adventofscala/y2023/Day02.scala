package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.util.matching.Regex

class Day02 extends FileLoader {
  def splitRounds(s: String): Iterable[String] = {
    s.split(":")(1).split(";")
  }

  def findNCubes(roundString: String): (Int, Int, Int) = {
    val blueCubes: Int =
      """(\d+) blue""".r
        .findFirstMatchIn(roundString)
        .map(_.group(1))
        .map(s => s.toInt)
        .getOrElse(0)

    val redCubes: Int =
      """(\d+) red""".r
        .findFirstMatchIn(roundString)
        .map(_.group(1))
        .map(s => s.toInt)
        .getOrElse(0)

    val greenCubes: Int =
      """(\d+) green""".r
        .findFirstMatchIn(roundString)
        .map(_.group(1))
        .map(s => s.toInt)
        .getOrElse(0)

    (redCubes, greenCubes, blueCubes)
  }

  def gamePossible(
      limits: (Int, Int, Int),
      game: Iterable[(Int, Int, Int)]
  ): Boolean = {

    game.forall(cubesInRound =>
      cubesInRound._1 <= limits._1 && cubesInRound._2 <= limits._2 && cubesInRound._3 <= limits._3
    )
  }

  def calcPower(game: Iterable[(Int, Int, Int)]): Int = {
    game.map(_._1).max * game.map(_._2).max * game.map(_._3).max
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList

    val answer = inputList
      .map(gameString =>
        splitRounds(gameString).map(roundString => findNCubes(roundString))
      )
      .map(game => gamePossible(limits = (12, 13, 14), game))
      .zipWithIndex
      .filter(_._1)
      .map(_._2 + 1)
      .sum

    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList

    val answer = inputList
      .map(gameString =>
        splitRounds(gameString)
          .map(roundString => findNCubes(roundString))
      )
      .map(calcPower)
      .sum

    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
