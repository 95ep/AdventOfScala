package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.collection.SortedSet
import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

class Day16 extends FileLoader {

  @tailrec
  final def findCorridors(
      inputList: List[String],
      i: Int,
      j: Int,
      corridors: Set[(Int, Int, Int)]
  ): Set[(Int, Int, Int)] = {
    if (i == inputList.size - 1) {
      corridors
    } else {
      val newNodes =
        if (inputList(i)(j) != '#')
          corridors ++ Range(0, 4).map(d => (i, j, d)).toSet
        else corridors
      val (iNew, jNew) = if (j == inputList.size - 1) (i + 1, 0) else (i, j + 1)
      findCorridors(inputList, iNew, jNew, newNodes)
    }

  }

  @tailrec
  final def dijkstas(
      unvisited: Set[(Int, Int, Int)],
      distances: Map[(Int, Int, Int), Long]
  ): Map[(Int, Int, Int), Long] = {
    if (unvisited.isEmpty) distances
    else {
      val (nextPos, nextDist) =
        unvisited.foldLeft(((0, 0, 0), Long.MaxValue))((carry, n) => {
          val d = distances(n)
          if (d < carry._2) (n, d) else carry
        })
      if (nextDist == Long.MaxValue) dijkstas(unvisited - nextPos, distances)
      else {
        val direction = nextPos._3
        val neighbours =
          List(
            (nextPos._1, nextPos._2, (direction + 1) % 4),
            (nextPos._1, nextPos._2, (direction + 3) % 4),
            if (direction == 0) (nextPos._1, nextPos._2 + 1, direction)
            else if (direction == 1) (nextPos._1 + 1, nextPos._2, direction)
            else if (direction == 2) (nextPos._1, nextPos._2 - 1, direction)
            else (nextPos._1 - 1, nextPos._2, direction)
          ).filter(p => distances.contains(p) && distances(p) == Long.MaxValue)
        val newDistances = neighbours.foldLeft(distances)((d, n) => {
          val tentativeDistance =
            if (n._3 == direction) nextDist + 1 else nextDist + 1000
          if (tentativeDistance < d(n)) d.updated(n, tentativeDistance) else d
        })
        dijkstas(unvisited ++ neighbours - nextPos, newDistances)
      }
    }
  }

  @tailrec
  final def findBestPaths(
      currentPaths: List[List[(Int, Int, Int)]],
      bestPaths: List[List[(Int, Int, Int)]],
      distances: Map[(Int, Int, Int), Long]
  ): List[List[(Int, Int, Int)]] = {
    if (currentPaths.isEmpty) bestPaths
    else {
      val pos = currentPaths.head.head
      val direction = pos._3
      val neighbours =
        List(
          (pos._1, pos._2, (direction + 1) % 4),
          (pos._1, pos._2, (direction + 3) % 4),
          if (direction == 0) (pos._1, pos._2 - 1, direction)
          else if (direction == 1) (pos._1 - 1, pos._2, direction)
          else if (direction == 2) (pos._1, pos._2 + 1, direction)
          else (pos._1 + 1, pos._2, direction)
        ).filter(n => distances.contains(n) && distances(n) < distances(pos))
      val newBest =
        if (neighbours.isEmpty) currentPaths.head :: bestPaths else bestPaths

      val newCurrent =
        neighbours.map(n => n :: currentPaths.head) ::: currentPaths.tail
      findBestPaths(newCurrent, newBest, distances)
    }
  }

  def part1(inputPath: String): Long = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val corridors = findCorridors(inputList, 0, 0, Set())

    val start = (inputList.size - 2, 1, 0)
    val distances: Map[(Int, Int, Int), Long] =
      corridors.map(k => k -> Long.MaxValue).toMap.updated(start, 0)
    val unvisited = Set(start)

    val shortestDistances = dijkstas(unvisited, distances)

    val finish =
      Range(0, 4).map(i => (1, inputList.size - 2, i))
    val answer = finish.map(p => shortestDistances(p)).min
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val corridors = findCorridors(inputList, 0, 0, Set())

    val start = (inputList.size - 2, 1, 0)
    val distances: Map[(Int, Int, Int), Long] =
      corridors.map(k => k -> Long.MaxValue).toMap.updated(start, 0)
    val unvisited = Set(start)

    val shortestDistances = dijkstas(unvisited, distances)

    val finish =
      Range(0, 4).map(i => (1, inputList.size - 2, i))
    val bestFinish =
      finish.sortBy(p => shortestDistances(p)).head

    val bestPaths =
      findBestPaths(List(List(bestFinish)), List(), shortestDistances)

    val answer = bestPaths.flatten.map(p => (p._1, p._2)).toSet.size
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
