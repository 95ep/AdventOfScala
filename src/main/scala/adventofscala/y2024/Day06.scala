package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day06 extends FileLoader {

  class Guard(position: (Int, Int), direction: (Int, Int)) {
    def pos = position
    def dir = direction
    def nextPos: (Int, Int) =
      (position._1 + direction._1, position._2 + direction._2)

    def turnRight: Guard = {
      val newDir = (direction._2, -direction._1)
      Guard(position, newDir)
    }
    def takeStep: Guard = { Guard(nextPos, direction) }

  }
  def getObstacles(inputList: List[String]): Set[(Int, Int)] = {
    (for
      i <- 0 until inputList.length
      j <- 0 until inputList(i).length
    yield if (inputList(i)(j) == '#') Some((i, j)) else None).flatten.toSet
  }

  def getGuard(inputList: List[String]): Guard = {
    val startPos = (for
      i <- 0 until inputList.length
      j <- 0 until inputList(i).length
    yield if (inputList(i)(j) == '^') Some((i, j)) else None).flatten.head

    Guard(startPos, (-1, 0))
  }

  @tailrec
  final def walkGuard(
      guard: Guard,
      obstacles: Set[(Int, Int)],
      visited: Set[(Int, Int)],
      size: Int
  ): Set[(Int, Int)] = {
    val nextPos = guard.nextPos
    if (
      nextPos._1 == size || nextPos._1 < 0 || nextPos._2 == size || nextPos._2 < 0
    ) visited + guard.pos
    else {
      val updatedGuard =
        if (obstacles.contains(nextPos)) guard.turnRight else guard

      walkGuard(updatedGuard.takeStep, obstacles, visited + guard.pos, size)
    }
  }

  @tailrec
  final def testLoop(
      guard: Guard,
      obstacles: Set[(Int, Int)],
      visited: Set[(Int, Int, Int, Int)],
      size: Int
  ): Boolean = {

    val nextPos = guard.nextPos
    if (
      nextPos._1 == size || nextPos._1 < 0 || nextPos._2 == size || nextPos._2 < 0
    ) false
    else {

      val updatedGuard =
        if (obstacles.contains(nextPos)) guard.turnRight else guard.takeStep

      val posAndDir = (
        updatedGuard.pos._1,
        updatedGuard.pos._2,
        updatedGuard.dir._1,
        updatedGuard.dir._2
      )
      if (visited.contains(posAndDir)) true
      else
        testLoop(
          updatedGuard,
          obstacles,
          visited + posAndDir,
          size
        )
    }
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val obstacles = getObstacles(inputList)
    val guard = getGuard(inputList)
    val visited = walkGuard(guard, obstacles, Set(), inputList.size)

    val answer = visited.size
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val obstacles = getObstacles(inputList)
    val guard = getGuard(inputList)
    val visited = walkGuard(guard, obstacles, Set(), inputList.size)
    val isLoop = (visited - guard.pos).toList
      .map(v => testLoop(guard, obstacles + v, Set(), inputList.size))
    val answer = isLoop.count(b => b)
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
