package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day21 extends FileLoader {

  @tailrec
  private def traceSteps(
      unvisited: Set[(Int, Int)],
      map: List[List[Char]],
      distances: Map[(Int, Int), Int],
      maxSteps: Int
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
        .filter(p => currentDist < maxSteps)

      val newDistances =
        neighbours.foldLeft(distances)((d, p) => d.updated(p, currentDist + 1))
      val newUnvisited = unvisited ++ neighbours - current
      traceSteps(newUnvisited, map, newDistances, maxSteps)
    }

  }

  def part1(inputPath: String, maxSteps: Int): Int = {
    println("Running part 1")
    val inputList: List[List[Char]] =
      loadLines(inputPath).toList.map(_.toCharArray().toList)

    val start =
      inputList
        .map(l => l.indexOf('S'))
        .zipWithIndex
        .filter(_._1 != -1)
        .map(p => (p._2, p._1))
        .head

    val distances = {
      for
        i <- 0 until inputList.size
        j <- 0 until inputList.head.size
      yield (i, j) -> Int.MaxValue
    }.toMap.updated(start, 0)
    val allDistances = traceSteps(Set(start), inputList, distances, maxSteps)
    val reached = allDistances.filter((_, d) => d < maxSteps + 1).toList
    val answer = allDistances.count((_, d) => d == maxSteps)
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    // val inputList: List[String] = loadLines(inputPath).toList

    val answer = 1
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
