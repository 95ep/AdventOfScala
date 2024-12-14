package adventofscala.y2024
import adventofscala.utils.FileLoader
import breeze.linalg.max
import scala.annotation.tailrec
import breeze.plot._

class Day14 extends FileLoader {

  def parse(line: String): (Int, Int, Int, Int) = {
    val pattern = raw"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)".r
    val m = pattern.findFirstMatchIn(line).get
    (m.group(1).toInt, m.group(2).toInt, m.group(3).toInt, m.group(4).toInt)
  }

  def calcPos(
      robot: (Int, Int, Int, Int),
      maxX: Int,
      maxY: Int,
      iter: Int
  ): (Int, Int) = {
    val newX = (robot._1 + robot._3 * iter) % maxX
    val newY = (robot._2 + robot._4 * iter) % maxY

    (if (newX < 0) maxX + newX else newX, if (newY < 0) maxY + newY else newY)
  }

  def calcSafetyFactor(robots: List[(Int, Int)], maxX: Int, maxY: Int): Int = {
    val xLim = (maxX - 1) / 2
    val yLim = (maxY - 1) / 2
    val quad1 = robots.filter((x, y) => x < xLim && y < yLim).size
    val quad2 = robots.filter((x, y) => x > xLim && y < yLim).size
    val quad3 = robots.filter((x, y) => x < xLim && y > yLim).size
    val quad4 = robots.filter((x, y) => x > xLim && y > yLim).size
    quad1 * quad2 * quad3 * quad4
  }

  def part1(inputPath: String, maxX: Int, maxY: Int): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val robots = inputList.map(parse(_))
    val newPos = robots.map(robot => calcPos(robot, maxX, maxY, 100))

    val answer = calcSafetyFactor(newPos, maxX, maxY)
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  final def iterAndPrint(
      robots: List[(Int, Int, Int, Int)],
      maxX: Int,
      maxY: Int,
      iter: Int
  ): Unit = {
    val newPos = robots.map(r => calcPos(r, maxX, maxY, iter))
    val f = Figure()
    val p = f.subplot(0)
    val x = newPos.map(_._1)
    val y = newPos.map(_._2)
    p += plot(x, y, '.')
    f.saveas(s"plots/$iter.png")
    f.clear()
  }

  def part2(inputPath: String, maxX: Int, maxY: Int): Unit = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val robots = inputList.map(parse(_))
    val maxIter = 103 * 101
    val allFactors = Range(0, maxIter).map(i =>
      val factor = calcSafetyFactor(
        robots.map(robot => calcPos(robot, maxX, maxY, i)),
        maxX,
        maxY
      )
      (i, factor)
    )
    val sorted = allFactors.sortBy((i, f) => f)

    sorted.take(20).map((i, _) => iterAndPrint(robots, maxX, maxY, i))
  }
}
