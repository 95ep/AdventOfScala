package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.*
import spire.syntax.group

class Day20 extends FileLoader {

  def parseTrack(
      inputList: List[String],
      end: (Int, Int)
  ): Map[(Int, Int), Int] = {
    val distances = (for
      (l, i) <- inputList.zipWithIndex
      (c, j) <- l.zipWithIndex if (c != '#')
    yield (i, j) -> Int.MaxValue).toMap
    distances.updated(end, 0)
  }

  @tailrec
  final def dijkstras(
      unvisited: Set[(Int, Int)],
      distances: Map[(Int, Int), Int],
      prev: Map[(Int, Int), (Int, Int)]
  ): (Map[(Int, Int), Int], Map[(Int, Int), (Int, Int)]) = {
    if (unvisited.isEmpty) (distances, prev)
    else {
      val pos = unvisited.toList.sortBy(c => distances(c)).head
      val dist = distances(pos)
      val (x, y) = pos
      val neighbours =
        List((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1))
          .filter(distances.contains(_))
      val updatedNeighbours =
        neighbours.filter(n => dist + 1 < distances(n))
      val newPrev =
        updatedNeighbours.foldLeft(prev)((p, n) => p.updated(n, pos))
      val newDistances =
        updatedNeighbours.foldLeft(distances)((d, n) => d.updated(n, dist + 1))

      val newUnvisited = unvisited ++ updatedNeighbours - pos
      dijkstras(
        newUnvisited,
        newDistances,
        newPrev
      )

    }
  }

  @tailrec
  final def shortestPath(
      current: (Int, Int),
      path: Seq[(Int, Int)],
      prev: Map[(Int, Int), (Int, Int)],
      goal: (Int, Int)
  ): Seq[(Int, Int)] = {
    if (current == goal) path :+ current
    else
      shortestPath(prev(current), path :+ current, prev, goal)
  }

  def getJumps(s: (Int, Int), maxDuration: Int): Seq[((Int, Int), Int)] = {
    val j = (for
      d <- 1 until maxDuration + 1
      hDiff <- 0 until d + 1
      vDiff = d - hDiff
    yield List(
      ((s._1 + vDiff, s._2 + hDiff), d),
      ((s._1 - vDiff, s._2 + hDiff), d),
      ((s._1 + vDiff, s._2 - hDiff), d),
      ((s._1 - vDiff, s._2 - hDiff), d)
    )).flatten.distinct
    j
  }

  def findCheatsSaved(
      distances: Map[(Int, Int), Int],
      shortestPath: Seq[(Int, Int)],
      maxDuration: Int
  ): Seq[Int] = {
    shortestPath
      .map(p => {
        getJumps(p, maxDuration)
          .filter((jumpTo, _) => distances.contains(jumpTo))
          .map((jumpTo, jumpDistance) => {
            distances(p) - (distances(jumpTo) + jumpDistance)
          })
      })
      .flatten

  }
  def part1(inputPath: String, start: (Int, Int), end: (Int, Int)): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val distance = parseTrack(inputList, end)

    val (dist, prev) = dijkstras(Set(end), distance, Map())

    val shortest = shortestPath(start, List(), prev, end)
    val savedWithCheats = findCheatsSaved(dist, shortest, 2).filter(_ > 99)

    val answer = savedWithCheats.size
    val groups =
      savedWithCheats.groupBy(k => k).mapValues(_.size).toMap
    groups.foreach((k, v) => println(s"$v cheats that saves $k nanos"))
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String, start: (Int, Int), end: (Int, Int)): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val distance = parseTrack(inputList, end)

    val (dist, prev) = dijkstras(Set(end), distance, Map())

    val shortest = shortestPath(start, List(), prev, end)
    val savedWithCheats = findCheatsSaved(dist, shortest, 20).filter(_ > 99)

    val answer = savedWithCheats.size
    val groups =
      savedWithCheats.groupBy(k => k).mapValues(_.size).toMap
    groups.foreach((k, v) => println(s"$v cheats that saves $k nanos"))
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
