package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day21 extends FileLoader {

  @tailrec
  private def traceSteps(
      unvisited: Set[(Int, Int)],
      map: List[List[Char]],
      distances: Map[(Int, Int), Int]
      // maxSteps: Int
  ): Map[(Int, Int), Int] = {
    if (unvisited.isEmpty) distances
    else {
      val current = unvisited.minBy(p => distances(p))
      val currentDist = distances(current)
      val neighbours = List(
        (current._1, current._2 + 1),
        (current._1, current._2 - 1),
        (current._1 + 1, current._2),
        (current._1 - 1, current._2)
      ).filter(p =>
        -1 < p._1 && p._1 < map.size && -1 < p._2 && p._2 < map.head.size
      ).filter(p => map(p._1)(p._2) != '#')
        .filter(p => !distances.keySet.contains(p))
      val newDistances =
        neighbours.foldLeft(distances)((d, p) => d.updated(p, currentDist + 1))
      val newUnvisited = unvisited ++ neighbours - current
      traceSteps(newUnvisited, map, newDistances)
    }

  }

  def getDistances(inputPath: String, maxSteps: Int): Map[(Int, Int), Int] = {
    val inputList: List[List[Char]] =
      loadLines(inputPath).toList.map(_.toCharArray().toList)

    val start =
      inputList
        .map(l => l.indexOf('S'))
        .zipWithIndex
        .filter(_._1 != -1)
        .map(p => (p._2, p._1))
        .head

    traceSteps(Set(start), inputList, Map(start -> 0))
  }

  def part1(inputPath: String, maxSteps: Int): Int = {
    println("Running part 1")
    val allDistances = getDistances(inputPath, maxSteps)
    val answer = allDistances.count((_, d) => d % 2 == 0 && d <= maxSteps)

    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Long = {
    println("Running part 2")
    val allDistances = getDistances(inputPath, 64)

    val oddCorners =
      allDistances.count((_, d) => d > 65 && d % 2 == 1)
    val evenCorners =
      allDistances.count((_, d) => d > 65 && d % 2 == 0)

    val oddFull = allDistances.count((_, d) => d % 2 == 1)
    val evenFull = allDistances.count((_, d) => d % 2 == 0)

    val n: Long = 2023 * 100L

    val answer =
      (n + 1) * (n + 1) * oddFull + n * n * evenFull - (n + 1) * oddCorners + n * evenCorners
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
