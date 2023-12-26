package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.util.matching.Regex
import breeze.linalg._
import scala.compiletime.ops.int

class Day24 extends FileLoader {

  case class Coordinate(x: Long, y: Long, z: Long) {
    def toVector: DenseVector[Double] = DenseVector[Double](x, y, z)
  }

  case class Hailstone(pos: Coordinate, vel: Coordinate) {
    def equationSystem(
        that: Hailstone
    ): (DenseMatrix[Double], DenseVector[Double]) = {
      val a =
        DenseMatrix(
          (vel.x.toDouble, -that.vel.x.toDouble),
          (vel.y.toDouble, -that.vel.y.toDouble)
        )
      val b =
        DenseVector[Double](that.pos.x - pos.x, that.pos.y - pos.y)
      (a, b)
    }

    def positionAtT(t: Double): DenseVector[Double] =
      pos.toVector + (vel.toVector *:* t)
  }

  def parseHailstone(line: String) = {
    val pattern: Regex = """(.+), (.+), (.+) @ (.+), (.+), (.+)""".r
    val m = pattern.findFirstMatchIn(line).get
    val pos =
      Coordinate(m.group(1).toLong, m.group(2).toLong, m.group(3).toLong)
    val vel =
      Coordinate(m.group(4).toLong, m.group(5).toLong, m.group(6).toLong)
    Hailstone(pos, vel)
  }

  def findIntersection(
      s1: Hailstone,
      s2: Hailstone
  ): Option[(Double, Double)] = {
    val eqSys = s1.equationSystem(s2)
    val a = eqSys._1
    val b = eqSys._2
    if (rank(a) == 2) {
      val t = a \ b
      if (t.forall(d => d > 0.0)) {
        val intersection = s1.positionAtT(t(0))
        Some((intersection(0), intersection(1)))
      } else None
    } else None
  }

  def withinTestArea(inter: (Double, Double), area: (Long, Long)): Boolean = {
    if (
      area._1 <= inter._1 && area._2 >= inter._1 && area._1 <= inter._2 && area._2 >= inter._2
    ) true
    else false
  }

  def part1(inputPath: String, testArea: (Long, Long)): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val hailstones: List[Hailstone] = inputList.map(parseHailstone)

    val inter = hailstones
      .combinations(2)
      .map(h => findIntersection(h.head, h(1)))
      .flatten
      .filter(i => withinTestArea(i, testArea))
    val answer = inter.size
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
