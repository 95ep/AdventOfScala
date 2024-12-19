package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day18 extends FileLoader {

  def parseObstacles(
      inputList: List[String],
      timeFrame: Int
  ): Set[(Int, Int)] = {
    (for
      i <- 0 until timeFrame
      coord = inputList(i).split(",").map(_.toInt)
    yield (coord(0), coord(1))).toSet
  }

  @tailrec
  final def dijkstras(
      unvisited: Seq[(Int, Int)],
      distances: Map[(Int, Int), Int],
      obstacles: Set[(Int, Int)],
      break: Boolean,
      prev: Map[(Int, Int), (Int, Int)],
      size: Int
  ): (Int, Map[(Int, Int), (Int, Int)]) = {
    val goalDistance = distances((size - 1, size - 1))
    if (goalDistance < Int.MaxValue) (goalDistance, prev)
    else if (break) (Int.MaxValue, prev)
    else {
      val pos = unvisited.sortBy(c => distances(c)).head
      val dist = distances(pos)
      val (x, y) = pos
      val neighbours =
        List((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1))
          .filter(c => !obstacles(c))
          .filter((x, y) => 0 <= x && x < size && 0 <= y && y < size)
      val updatedNeighbours =
        neighbours.filter(n => dist + 1 < distances(n))
      val newPrev =
        updatedNeighbours.foldLeft(prev)((p, n) => p.updated(n, pos))
      val newDistances =
        updatedNeighbours.foldLeft(distances)((d, n) => d.updated(n, dist + 1))
      val shouldBreak = dist == Int.MaxValue
      dijkstras(
        unvisited.filter(_ != pos),
        newDistances,
        obstacles,
        shouldBreak,
        newPrev,
        size
      )

    }
  }

  @tailrec
  final def shortestPath(
      current: (Int, Int),
      path: Seq[(Int, Int)],
      prev: Map[(Int, Int), (Int, Int)]
  ): Seq[(Int, Int)] = {
    if (current == (0, 0)) path :+ current
    else
      shortestPath(prev(current), path :+ current, prev)
  }

  @tailrec
  final def findCutOffTime(
      timeFrame: Int,
      inputList: List[String],
      size: Int
  ): Int = {
    println(s"Testing timeframe $timeFrame")
    val obstacles = parseObstacles(inputList, timeFrame)
    val allPositions = (for
      i <- 0 until size
      j <- 0 until size
    yield (i, j))

    val distances =
      allPositions.map(c => c -> Int.MaxValue).toMap.updated((0, 0), 0)

    val (dist, allPrev) =
      dijkstras(allPositions, distances, obstacles, false, Map(), size)

    if (dist == Int.MaxValue) timeFrame - 1
    else
      val path = shortestPath((size - 1, size - 1), Seq(), allPrev)

      val time = inputList
        .map(s => {
          val c = s.split(",").map(_.toInt)
          (c(0), c(1))
        })
        .zipWithIndex
        .filter((c, i) => path.contains(c))
        .sortBy(_._2)
        .head
        ._2 + 1

      findCutOffTime(time, inputList, size)
  }

  def part1(inputPath: String, timeFrame: Int, size: Int): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val obstacles = parseObstacles(inputList, timeFrame)
    val allPositions = (for
      i <- 0 until size
      j <- 0 until size
    yield (i, j))

    val distances =
      allPositions.map(c => c -> Int.MaxValue).toMap.updated((0, 0), 0)

    val answer =
      dijkstras(allPositions, distances, obstacles, false, Map(), size)._1
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String, timeFrame: Int, size: Int): String = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val cutoffTime = findCutOffTime(timeFrame, inputList, size)

    val answer = inputList(cutoffTime)
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
