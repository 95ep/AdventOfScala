package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.collection.immutable.HashSet

class Day10 extends FileLoader {

  case class Sketch(m: Array[Array[Char]]) {
    def checkIf(c: Char, i: (Int, Int)): Boolean = {
      if (0 <= i._1 && i._1 < m.size && 0 <= i._2 && i._2 < m(0).size) {
        m(i._1)(i._2) == c
      } else
        false
    }

    def get(i: (Int, Int)): Char = {
      if (0 <= i._1 && i._1 < m.size && 0 <= i._2 && i._2 < m(0).size) {
        m(i._1)(i._2)
      } else
        throw RuntimeException("error")
    }
  }

  val NORTH_CONNECTIONS = List('|', '7', 'F')
  val EAST_CONNECTIONS = List('-', 'J', '7')
  val SOUTH_CONNECTIONS = List('|', 'L', 'J')
  val WEST_CONNECTIONS = List('-', 'L', 'F')

  val PIPE_CONNECTIONS: Map[
    Char,
    (List[Char], List[Char], List[Char], List[Char])
  ] = Map(
    '|' -> (NORTH_CONNECTIONS, List(), SOUTH_CONNECTIONS, List()),
    '-' -> (List(), EAST_CONNECTIONS, List(), WEST_CONNECTIONS),
    'L' -> (NORTH_CONNECTIONS, EAST_CONNECTIONS, List(), List()),
    'J' -> (NORTH_CONNECTIONS, List(), List(), WEST_CONNECTIONS),
    '7' -> (List(), List(), SOUTH_CONNECTIONS, WEST_CONNECTIONS),
    'F' -> (List(), EAST_CONNECTIONS, SOUTH_CONNECTIONS, List()),
    'S' -> (NORTH_CONNECTIONS, EAST_CONNECTIONS, SOUTH_CONNECTIONS, WEST_CONNECTIONS)
  )

  val VERTICAL_CHARS = List(
    '|',
    'L',
    'J'
    // '7',
    // 'F'
  )

  def findLoop(sketch: Sketch, start: (Int, Int)): List[(Int, Int)] = {
    var done = false
    var loop: List[(Int, Int)] = List()
    var current: (Int, Int) = start
    while (!done) {
      // Thread.sleep(500)
      loop = loop.appended(current)
      val currentChar = sketch.get(current)
      // println(s"Current: $current -> char $currentChar")

      val validNeighbours: (List[Char], List[Char], List[Char], List[Char]) =
        PIPE_CONNECTIONS(currentChar)
      val north = (current._1 - 1, current._2)
      val east = (current._1, current._2 + 1)
      val west = (current._1, current._2 - 1)
      val south = (current._1 + 1, current._2)

      if (
        !loop.contains(north) && validNeighbours._1
          .map(sketch.checkIf(_, north))
          .exists(b => b)
      ) {
        current = north
      } else if (
        !loop.contains(east) &&
        validNeighbours._2.map(sketch.checkIf(_, east)).exists(b => b)
      ) { current = east }
      else if (
        !loop.contains(south) &&
        validNeighbours._3.map(sketch.checkIf(_, south)).exists(b => b)
      ) { current = south }
      else if (
        !loop.contains(west) && validNeighbours._4
          .map(sketch.checkIf(_, west))
          .exists(b => b)
      ) {
        current = west
      } else { done = true }

    }

    loop
  }

  def findStart(sketch: Array[Array[Char]]): (Int, Int) = {
    sketch.map(_.indexOf('S')).zipWithIndex.filter((a, _) => a > -1).head.swap
  }

  def isInsideLoop(
      p: (Int, Int),
      loop: HashSet[(Int, Int)],
      rows: Int,
      columns: Int,
      sketch: Sketch
  ): Boolean = {
    if (p == (3, 14)) {
      println()
    }
    if (loop.contains(p)) false
    else {
      val rangeColToRight = Range(p._2, columns).map(col => (p._1, col))
      rangeColToRight
        .filter(point => {
          loop.contains(point) && VERTICAL_CHARS.contains(sketch.get(point))
        })
        .size % 2 == 1
    }
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val input: Array[Array[Char]] =
      loadLines(inputPath).toArray.map(_.toCharArray())

    val start = findStart(input)
    val sketch = Sketch(input)
    val answer = findLoop(sketch, start).size / 2
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val input: Array[Array[Char]] =
      loadLines(inputPath).toArray.map(_.toCharArray())

    val start = findStart(input)
    val sketch = Sketch(input)
    val loop = HashSet() ++ findLoop(sketch, start)
    println(s"Loop size ${loop.size} ")
    val columns = input(0).size
    val rows = input.size
    val possiblePoints =
      Range(0, rows)
        .flatMap(x => Range(0, columns).map(y => (x, y)))
        .toList

    val answer =
      possiblePoints
        .map(p => isInsideLoop(p, loop, rows, columns, sketch))
        .count(b => b)
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
