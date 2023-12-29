package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day22 extends FileLoader {

  case class Coordinate(x: Int, y: Int, z: Int) {
    def coordDown: Coordinate = Coordinate(x, y, z - 1)
  }

  case class Brick(idx: Int, start: Coordinate, end: Coordinate) {
    def moveDown: Brick = Brick(idx, start.coordDown, end.coordDown)
    def getCoords: List[Coordinate] =
      if (start.x != end.x)
        Range(start.x, end.x + 1).map(Coordinate(_, start.y, start.z)).toList
      else if (start.y != end.y) {
        Range(start.y, end.y + 1).map(Coordinate(start.x, _, start.z)).toList
      } else
        Range(start.z, end.z + 1).map(Coordinate(start.x, start.y, _)).toList
    def getCoordsBelow: List[Coordinate] = getCoords.map(_.coordDown)
    def canMoveDown: Boolean = minZ > 1
    def minZ = getCoords.map(_.z).min
  }

  def parseBrick(line: String, idx: Int): Brick = {
    val start = line.split("~")(0).split(",").map(_.toInt)
    val end = line.split("~")(1).split(",").map(_.toInt)
    Brick(
      idx,
      Coordinate(start(0), start(1), start(2)),
      Coordinate(end(0), end(1), end(2))
    )
  }

  @tailrec
  private def fallingBricks(
      bricksList: List[Brick],
      occupiedSpace: Map[Coordinate, Brick]
  ): (List[Brick], Map[Coordinate, Brick]) = {

    val newAccum =
      bricksList
        .sortBy(_.minZ)
        .foldLeft((List[Brick](), Map[Coordinate, Brick]()))((accum, brick) =>
          if (
            brick.canMoveDown && !brick.getCoordsBelow
              .exists(c => accum._2.contains(c))
          ) {
            val movedBrick = brick.moveDown
            (
              movedBrick :: accum._1,
              movedBrick.getCoords.foldLeft(accum._2)((occupied, c2) =>
                occupied.updated(c2, movedBrick)
              )
            )
          } else {
            (
              brick :: accum._1,
              brick.getCoords.foldLeft(accum._2)((occupied, c2) =>
                occupied.updated(c2, brick)
              )
            )
          }
        )
    if (newAccum._2.keySet == occupiedSpace.keySet) newAccum
    else
      fallingBricks(newAccum._1, newAccum._2)
  }

  private def findSupport(
      bricksList: List[Brick],
      occupiedSpace: Map[Coordinate, Brick]
  ): (Map[Brick, Set[Brick]], Map[Brick, Set[Brick]]) = {
    val supportedBy: Map[Brick, Set[Brick]] = Map()
    val supporting: Map[Brick, Set[Brick]] = Map()
    bricksList.foldLeft((supportedBy, supporting))((support, brick) =>
      val bricksBelow =
        brick.getCoordsBelow
          .map(occupiedSpace.get(_))
          .flatten
          .toSet
          .filter(brickBelow => brickBelow != brick)
      val newSupportedBy = support._1.updated(brick, bricksBelow)
      val newSupporting =
        bricksBelow.foldLeft(support._2)((supportingTmp, supportBrick) => {
          val newSet = supportingTmp.getOrElse(supportBrick, Set()) + brick
          supportingTmp.updated(supportBrick, newSet)
        })
      (newSupportedBy, newSupporting)
    )
  }

  def bricksToRemove(
      brickList: List[Brick],
      supportedBy: Map[Brick, Set[Brick]],
      supporting: Map[Brick, Set[Brick]]
  ): List[Brick] = {
    brickList
      .filter(brick => {
        supporting
          .getOrElse(brick, Set())
          .filter(supportedBy(_).size == 1)
          .isEmpty
      })
  }

  def nFalling(removedBricks: List[Brick], restingBricks: List[Brick]): Int = {
    removedBricks
      .map(brick => {
        val newResting = restingBricks.filter(_ != brick)
        val afterFalling = fallingBricks(newResting, Map())._1
        val fallen = afterFalling.filter(b => !newResting.contains(b))
        fallen.size
      })
      .sum
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val bricks: List[Brick] =
      loadLines(inputPath).toList.zipWithIndex.map((l, i) => parseBrick(l, i))
    val result = fallingBricks(bricks, Map())
    val restingBricks = result._1
    val occupiedSpace = result._2
    val support = findSupport(restingBricks, occupiedSpace)

    val answer = bricksToRemove(restingBricks, support._1, support._2).size
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val bricks: List[Brick] =
      loadLines(inputPath).toList.zipWithIndex.map((l, i) => parseBrick(l, i))
    val result = fallingBricks(bricks, Map())
    val restingBricks = result._1
    val occupiedSpace = result._2
    val support = findSupport(restingBricks, occupiedSpace)
    val noFallBricks =
      bricksToRemove(restingBricks, support._1, support._2)

    val answer = nFalling(
      restingBricks.filter(b => !noFallBricks.contains(b)),
      restingBricks
    )
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
