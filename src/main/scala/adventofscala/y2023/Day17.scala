package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import math.Ordered.orderingToOrdered
import scala.collection.immutable.TreeSet

class Day17 extends FileLoader {

  private def getNeightbours(
      node: (Int, Int, Int),
      rows: Int,
      columns: Int,
      part1: Boolean
  ): List[(Int, Int, Int)] = {
    val minDist = if (part1) 1 else 4
    val maxDist = if (part1) 3 else 10
    Range(minDist, maxDist + 1)
      .flatMap(f =>
        List(
          (0, (0, f)),
          (1, (f, 0)),
          (2, (0, -f)),
          (3, (-f, 0))
        )
      )
      .filter((dir, _) => dir != (node._3 + 2) % 4)
      .filter((dir, _) => dir != node._3)
      .map((dir, delta) => ((node._1 + delta._1, node._2 + delta._2, dir)))
      .filter(newNode =>
        Range(0, rows).contains(newNode._1) && Range(0, columns)
          .contains(newNode._2)
      )
      .toList
  }

  case class UnvisitedNode(node: (Int, Int, Int), cost: Int)
      extends Ordered[UnvisitedNode] {
    def compare(that: UnvisitedNode): Int =
      if (this.cost == that.cost) this.node.compare(that.node)
      else this.cost.compare(that.cost)
  }

  @tailrec
  private def search(
      distances: Map[(Int, Int, Int), Int],
      unvisited: SortedSet[UnvisitedNode],
      blockCosts: List[List[Int]],
      goal: (Int, Int),
      part1: Boolean
  ): Int = {
    val nRows = blockCosts.size
    val nCols = blockCosts.head.size
    if (unvisited.isEmpty) {
      Range(0, 4)
        .map(i => distances(nRows - 1, nCols - 1, i))
        .min
    } else {
      val currentNode = unvisited.head
      val neighbours: List[(Int, Int, Int)] =
        getNeightbours(
          currentNode.node,
          nRows,
          nCols,
          part1
        )

      val newUnvisited =
        neighbours
          .map(n => {
            val rows = List(currentNode.node._1, n._1)
            val cols = List(currentNode.node._2, n._2)
            val visited = {
              for
                i <- rows.min until rows.max + 1
                j <- cols.min until cols.max + 1
              yield (i, j)
            }.diff(List((currentNode.node._1, currentNode.node._2)))

            val addedDist = visited.map(i => blockCosts(i._1)(i._2)).sum

            val newDist = distances(currentNode.node) + addedDist
            if (newDist < distances(n)) Some(UnvisitedNode(n, newDist))
            else None
          })
          .flatten

      val newDistances: Map[(Int, Int, Int), Int] =
        newUnvisited.foldLeft(distances)((d, n) => {
          d.updated(n.node, n.cost)
        })

      search(
        newDistances,
        (unvisited ++ newUnvisited) - currentNode,
        blockCosts,
        goal,
        part1
      )
    }
  }

  def solution(inputList: List[String], part1: Boolean) = {
    val blockCosts: List[List[Int]] =
      inputList.map(_.toCharArray().map(_.asDigit).toList)

    val distancesRaw = {
      for
        i1 <- 0 until blockCosts.size
        i2 <- 0 until blockCosts.head.size
        i3 <- 0 until 4
      yield (i1, i2, i3) -> Int.MaxValue
    }.toMap

    val unvisited: SortedSet[UnvisitedNode] =
      TreeSet(
        UnvisitedNode((0, 0, 2), 0),
        UnvisitedNode((0, 0, 3), 0)
      )

    val distances =
      unvisited.foldLeft(distancesRaw)((d, n) => d.updated(n.node, 0))

    search(
      distances,
      unvisited,
      blockCosts,
      (blockCosts.size - 1, blockCosts.head.size - 1),
      part1
    )
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList

    val answer = solution(inputList, true)
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList

    val answer = solution(inputList, false)
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
