package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day08 extends FileLoader {

  def parseAntennas(input: List[String]): Map[Char, Set[(Int, Int)]] = {
    val allAntennas = input.zipWithIndex.flatMap((l, i) =>
      (l.zipWithIndex.map((c, j) => (c, (i, j))))
    )

    allAntennas.foldLeft(Map[Char, Set[(Int, Int)]]())((m, antenna) => {
      val c = antenna._1
      if (c != '.') {
        val s = m.getOrElse(c, Set())
        m + (c -> (s + antenna._2))
      } else m
    })
  }

  @tailrec
  final def findAntionodes(
      antenna: (Int, Int),
      delta: (Int, Int),
      size: Int,
      k: Int,
      accumulator: Set[(Int, Int)]
  ): Set[(Int, Int)] = {
    val nextAntinode = (antenna._1 + k * delta._1, antenna._2 + k * delta._2)
    if (
      !(nextAntinode._1 > -1 && nextAntinode._1 < size && nextAntinode._2 > -1 && nextAntinode._2 < size)
    ) { accumulator }
    else {
      findAntionodes(antenna, delta, size, k + 1, accumulator + nextAntinode)
    }
  }

  def calcDelta(antenna1: (Int, Int), antenna2: (Int, Int)) = {
    (antenna2._1 - antenna1._1, antenna2._2 - antenna1._2)
  }

  def findAllAntinodes(antennaLoc: Set[(Int, Int)]): Set[(Int, Int)] = {
    (for
      a1 <- antennaLoc
      a2 <- antennaLoc if a1 != a2
    yield {
      val delta = calcDelta(a1, a2)
      val antinode1 = (a1._1 - delta._1, a1._2 - delta._2)
      val antinode2 = (a2._1 + delta._1, a2._2 + delta._2)
      Set(antinode1, antinode2)
    }).flatten
  }

  def findAllAntinodesV2(
      antennaLoc: Set[(Int, Int)],
      size: Int
  ): Set[(Int, Int)] = {
    (for
      a1 <- antennaLoc
      a2 <- antennaLoc if a1 != a2
    yield {
      val delta = calcDelta(a1, a2)
      val antinodes = findAntionodes(a1, delta, size, 0, Set())
      val negativeDelta = (delta._1 * -1, delta._2 * -1)
      findAntionodes(a1, negativeDelta, size, 0, antinodes)
    }).flatten
  }

  def withinMap(antinode: (Int, Int), size: Int): Boolean = {
    val x = antinode._1
    val y = antinode._2
    x > -1 && x < size && y > -1 && y < size
  }
  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val antennasMap = parseAntennas(inputList)

    val antinodes = antennasMap
      .flatten((c, s) => findAllAntinodes(s))
      .filter(an => withinMap(an, inputList.size))
      .toSet

    val answer = antinodes.size
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val antennasMap = parseAntennas(inputList)

    val antinodes = antennasMap
      .flatten((c, s) => findAllAntinodesV2(s, inputList.size))
      .toSet

    val answer = antinodes.size
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
