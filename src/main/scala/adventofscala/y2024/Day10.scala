package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day10 extends FileLoader {

  @tailrec
  final def findTrailheadScore(
      positions: List[(Int, Int)],
      map: List[String],
      part2: Boolean
  ): Int = {
    val completed = positions.filter((i, j) => map(i)(j) == '9')
    if (completed == positions || positions.isEmpty) {
      if (part2)
        positions.size
      else positions.toSet.size
    } else {
      val validNeighbours = positions.flatMap((i, j) =>
        List((i + 1, j), (i, j + 1), (i - 1, j), (i, j - 1)).filter((m, n) =>
          m > -1 && n > -1 && m < map.size && n < map.size && map(i)(
            j
          ) + 1 == map(m)(
            n
          )
        )
      )
      findTrailheadScore(validNeighbours, map, part2)
    }

  }

  def findAllTrailheads(map: List[String]): List[(Int, Int)] = {
    (for
      i <- 0 until map.size
      j <- 0 until map.size if map(i)(j) == '0'
    yield (i, j)).toList
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val trailheads = findAllTrailheads(inputList)
    val scores =
      trailheads.map(h => findTrailheadScore(List(h), inputList, false))

    val answer = scores.sum
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val trailheads = findAllTrailheads(inputList)
    val scores =
      trailheads.map(h => findTrailheadScore(List(h), inputList, true))

    val answer = scores.sum

    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
