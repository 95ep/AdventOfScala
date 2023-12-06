package adventofscala.y2023
import adventofscala.utils.FileLoader

class Day06 extends FileLoader {

  def parseNumbers(s: String): List[String] = {
    """(\d+)""".r.findAllMatchIn(s).map(m => m.group(1)).toList
  }

  def parseRaces(s: List[String]): List[(Long, Long)] = {
    val times = parseNumbers(s.head).map(s => s.toLong)
    val distances = parseNumbers(s(1)).map(s => s.toLong)
    times.zip(distances)
  }

  def parseSingleRace(s: List[String]): (Long, Long) = {
    val time = parseNumbers(s.head).mkString.toLong
    val distance = parseNumbers(s(1)).mkString.toLong
    (time, distance)
  }

  def findWins(race: (Long, Long), direction: Long): Long = {
    var pressingTime = if (direction < 0) race._1 / 2 - 1 else race._1 / 2
    var nWinns = 0
    var stillWinning = true
    while (stillWinning) {
      var score = pressingTime * (race._1 - pressingTime)
      if (score > race._2) {
        nWinns += 1
        pressingTime += direction
      } else stillWinning = false
    }
    nWinns
  }

  def part1(inputPath: String): Long = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val races = parseRaces(inputList)
    val answer =
      races.map(race => { findWins(race, 1) + findWins(race, -1) }).product

    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Long = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val race = parseSingleRace(inputList)
    val answer = (findWins(race, 1) + findWins(race, -1))
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
