package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.util.matching.Regex
import breeze.linalg._
import scala.compiletime.ops.int
import scala.annotation.tailrec

class Day24 extends FileLoader {

  case class Coordinate(x: Long, y: Long, z: Long) {
    def toVector: DenseVector[Double] = DenseVector[Double](x, y, z)
    def toVectorLong: DenseVector[Long] = DenseVector[Long](x, y, z)
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

  def crossWithP(vec: DenseVector[Double]): DenseMatrix[Double] = {
    val mat = DenseMatrix.zeros[Double](3, 6)
    mat.update(0, 2, vec(1))
    mat.update(0, 1, -vec(2))

    mat.update(1, 0, vec(2))
    mat.update(1, 2, -vec(0))

    mat.update(2, 1, vec(0))
    mat.update(2, 0, -vec(1))
    mat
  }

  def crossWithV(vec: DenseVector[Double]): DenseMatrix[Double] = {
    val mat = DenseMatrix.zeros[Double](3, 6)
    mat.update(0, 5, vec(1))
    mat.update(0, 4, -vec(2))

    mat.update(1, 3, vec(2))
    mat.update(1, 5, -vec(0))

    mat.update(2, 4, vec(0))
    mat.update(2, 3, -vec(1))
    mat
  }

  def cross(
      stone: Hailstone
  ): DenseVector[Double] =
    val v1 = stone.pos.toVector
    val v2 = stone.vel.toVector
    val vec = DenseVector.zeros[Double](3)
    vec.update(0, v1(1) * v2(2) - v1(2) * v2(1))
    vec.update(1, v1(2) * v2(0) - v1(0) * v2(2))
    vec.update(2, v1(0) * v2(1) - v1(1) * v2(0))
    vec

  def prodForHailstone(hailstone: Hailstone): DenseMatrix[Double] = {
    val pos = hailstone.pos.toVector
    val vel = hailstone.vel.toVector
    crossWithP(vel) - crossWithV(pos)
  }

  def createVec(axis: Int, idx: Int, nStones: Int): DenseVector[Double] = {
    val vec = DenseVector
      .zeros[Double](nStones * 3 + 3)
    vec.update(axis, 1)
    vec.update(3 * idx + 3 + axis, 1)
    vec
  }

  def createA(hailstones: List[Hailstone]): DenseMatrix[Double] = {
    val prod0 = prodForHailstone(hailstones(0))
    val prod1 = prodForHailstone(hailstones(1))
    val prod2 = prodForHailstone(hailstones(2))

    val diff1 = prod1 - prod0
    val diff2 = prod2 - prod0

    DenseMatrix.vertcat(diff1, diff2)
  }

  def createB(hailstones: List[Hailstone]): DenseVector[Double] = {
    val diff1 = cross(hailstones(1)) - cross(hailstones(0))
    val diff2 = cross(hailstones(2)) - cross(hailstones(0))
    -DenseVector.vertcat(diff1, diff2)
  }

  def dist(delta: DenseVector[Long]): Long =
    delta.map(d => Math.abs(d)).toScalaVector.sum

  @tailrec
  private def findPos(
      stride: Long,
      delta: DenseVector[Long],
      deltaVel: DenseVector[Long],
      bestDelta: DenseVector[Long]
  ): DenseVector[Long] = {
    val newDelta = delta + (deltaVel *:* stride)
    val newBestDelta =
      if (dist(newDelta) < dist(bestDelta)) newDelta else bestDelta
    if (dist(newDelta) < dist(delta))
      findPos(stride, newDelta, deltaVel, newDelta)
    else if (stride == 1L) bestDelta
    else findPos(stride / 2, newDelta, -deltaVel, bestDelta)

  }

  def part2(inputPath: String): Long = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val hailstones: List[Hailstone] = inputList.map(parseHailstone)

    val a = createA(hailstones)
    val b: DenseVector[Double] = createB(hailstones)

    val sol = a \ b
    val approxPos = sol.slice(0, 3).map(Math.round(_))
    println(s"approx pos $approxPos")
    val vel = sol.slice(3, 6).map(Math.round(_))

    val stone0 = hailstones.head
    val delta =
      findPos(
        100000000000L,
        stone0.pos.toVectorLong - approxPos,
        stone0.vel.toVectorLong - vel,
        stone0.pos.toVectorLong - approxPos
      )
    println(s"Delta = $delta")
    val adjustedPos = approxPos + delta

    println(s"x,y,z=$adjustedPos")
    val answer: Long = adjustedPos.toScalaVector.sum
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
