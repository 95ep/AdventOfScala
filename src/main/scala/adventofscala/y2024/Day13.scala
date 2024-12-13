package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day13 extends FileLoader {
  val buttonPattern = raw".*X\+(\d+), Y\+(\d+)".r
  val prizePattern = raw".*X=(\d+), Y=(\d+)".r

  case class Machine(a: (Int, Int), b: (Int, Int), price: (Long, Long)) {
    def withPart2Prize: Machine =
      Machine(a, b, (price._1 + 10000000000000L, price._2 + 10000000000000L))

    def posAfterPush(one: Long, two: Long): (Long, Long) =
      (a._1 * one + b._1 * two, a._2 * one + b._2 * two)

    def onPrize(one: Long, two: Long): Boolean =
      val pos = posAfterPush(one, two)
      pos._1 == price._1 && pos._2 == price._2

    def asMatrix: SquareMatrix = SquareMatrix(a._1, b._1, a._2, b._2)

  }

  case class SquareMatrix(a: Int, b: Int, c: Int, d: Int) {
    def determinant: Int = a * d - b * c
    def adj: SquareMatrix = SquareMatrix(d, -b, -c, a)
    def multByVec(v: (Long, Long)): (Long, Long) = {
      (a * v._1 + b * v._2, c * v._1 + d * v._2)
    }

  }

  def parseMachines(input: List[String]): Seq[Machine] = {
    for i <- 0 until input.size by 4
    yield {
      val a = (
        buttonPattern.findFirstMatchIn(input(i)).get.group(1).toInt,
        buttonPattern.findFirstMatchIn(input(i)).get.group(2).toInt
      )
      val b = (
        buttonPattern.findFirstMatchIn(input(i + 1)).get.group(1).toInt,
        buttonPattern.findFirstMatchIn(input(i + 1)).get.group(2).toInt
      )
      val price = (
        prizePattern.findFirstMatchIn(input(i + 2)).get.group(1).toLong,
        prizePattern.findFirstMatchIn(input(i + 2)).get.group(2).toLong
      )
      Machine(a, b, price)
    }
  }

  def solveSystem(machine: Machine): Option[(Long, Long)] = {
    val matrix = machine.asMatrix
    val rawSol = matrix.adj.multByVec(machine.price)
    val proposedSolution =
      (rawSol._1 / matrix.determinant, rawSol._2 / matrix.determinant)
    if (machine.onPrize(proposedSolution._1, proposedSolution._2))
      Some(proposedSolution._1, proposedSolution._2)
    else None
  }

  def cost(solution: (Long, Long)): Long = solution._1 * 3 + solution._2

  def part1(inputPath: String): Long = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val machines = parseMachines(inputList)
    val allChepeast =
      machines
        .map(solveSystem(_))
        .flatten
        .filter((i, j) => i < 101 && j < 101)
        .map(cost(_))

    val answer = allChepeast.sum
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Long = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val machines = parseMachines(inputList).map(_.withPart2Prize)
    val allCosts = machines.map(solveSystem(_)).flatten.map(cost(_))

    val answer = allCosts.sum
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
