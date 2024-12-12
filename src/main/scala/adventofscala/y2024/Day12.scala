package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec
import breeze.numerics.sin

class Day12 extends FileLoader {

  def getNeighbours(candidate: (Int, Int), size: Int): List[(Int, Int)] = {
    val x = candidate._1
    val y = candidate._2
    List((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)).filter((i, j) =>
      i > -1 && i < size && j > -1 && j < size
    )
  }

  @tailrec
  final def createRegion(
      gardenMap: List[String],
      unvisited: Set[(Int, Int)],
      region: Set[(Int, Int)]
  ): Set[(Int, Int)] = {
    if (unvisited.isEmpty) region
    else {
      val candidate = unvisited.head
      val inRegion = region.head
      if (
        gardenMap(candidate._1)(candidate._2) == gardenMap(inRegion._1)(
          inRegion._2
        )
      ) {
        val newNeighbours = getNeighbours(candidate, gardenMap.size).filter(n =>
          !region.contains(n)
        )
        createRegion(
          gardenMap,
          unvisited ++ newNeighbours - candidate,
          region + candidate
        )

      } else createRegion(gardenMap, unvisited - candidate, region)
    }

  }

  @tailrec
  final def findRegions(
      gardenMap: List[String],
      regions: List[Set[(Int, Int)]],
      unvisited: Set[(Int, Int)]
  ): List[Set[(Int, Int)]] = {
    if (unvisited.isEmpty) regions
    else {
      val regionSeed = unvisited.head
      val unvisitedNeighbours = getNeighbours(regionSeed, gardenMap.size).toSet
      val newRegion =
        createRegion(gardenMap, unvisitedNeighbours, Set(regionSeed))
      val newUnvisited = unvisited.filter(t => !newRegion.contains(t))
      findRegions(gardenMap, regions :+ newRegion, newUnvisited)
    }
  }

  def calcRegionCost(region: Set[(Int, Int)], size: Int): Int = {
    val perimiterSize = region.toList
      .map(t => {
        List(
          (t._1 + 1, t._2),
          (t._1 - 1, t._2),
          (t._1, t._2 + 1),
          (t._1, t._2 - 1)
        )
          .filter(n => !region.contains(n))
          .size
      })
      .sum
    perimiterSize * region.size
  }

  @tailrec
  final def findSides(
      perimiter: Set[(Int, Int, String)],
      sides: List[Set[(Int, Int, String)]]
  ): List[Set[(Int, Int, String)]] = {
    if (perimiter.isEmpty) sides
    else {
      val p = perimiter.head
      val verticalRunning = p._3 == "S" || p._3 == "N"
      val sideNeighbours =
        if (!verticalRunning)
          List((p._1 + 1, p._2, p._3), (p._1 - 1, p._2, p._3))
        else List((p._1, p._2 + 1, p._3), (p._1, p._2 - 1, p._3))
      if (sides.exists(side => sideNeighbours.exists(n => side.contains(n)))) {
        val newSides =
          sides.foldLeft(List[Set[(Int, Int, String)]]())((l, side) => {
            if (sideNeighbours.exists(n => side.contains(n))) l :+ (side + p)
            else l :+ side
          })
        findSides(perimiter - p, newSides)
      } else {
        val newSides = sides :+ Set(p)
        findSides(perimiter - p, newSides)
      }
    }
  }

  @tailrec
  final def deduplicateSides(
      sides: Set[Set[(Int, Int, String)]]
  ): Set[Set[(Int, Int, String)]] = {
    val newSides = sides
      .flatMap(side =>
        val t2 = side.flatMap(p => {
          val verticalRunning = p._3 == "E" || p._3 == "W"
          val setWithPotentialNeighbours =
            if (verticalRunning)
              Set((p._1 + 1, p._2, p._3), (p._1 - 1, p._2, p._3))
            else Set((p._1, p._2 + 1, p._3), (p._1, p._2 - 1, p._3))
          val overlapping = (sides - side).filter(s =>
            !s.intersect(setWithPotentialNeighbours).isEmpty
          )
          val t =
            if (overlapping.isEmpty) Set(side)
            else overlapping.map(o => o ++ side)
          t
        })
        t2
      )
      .filter(s2 => !s2.isEmpty)
    if (newSides == sides) sides
    else deduplicateSides(newSides)
  }

  def calcDiscountedCost(region: Set[(Int, Int)], size: Int): Int = {
    val perimiter = region
      .flatMap(t => {
        List(
          (t._1 + 1, t._2, "S"),
          (t._1 - 1, t._2, "N"),
          (t._1, t._2 + 1, "E"),
          (t._1, t._2 - 1, "W")
        )
          .filter(n => !region.contains((n._1, n._2)))
      })
    val sides = findSides(perimiter, List())
    val dedup = deduplicateSides(sides.toSet)
    dedup.size * region.size

  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val unvisited = for
      i <- 0 until inputList.size
      j <- 0 until inputList.size
    yield (i, j)

    val regions = findRegions(inputList, List(), unvisited.toSet)

    val answer = regions.map(calcRegionCost(_, inputList.size)).sum
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val unvisited = for
      i <- 0 until inputList.size
      j <- 0 until inputList.size
    yield (i, j)

    val regions = findRegions(inputList, List(), unvisited.toSet)

    val answer = regions.map(calcDiscountedCost(_, inputList.size)).sum
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
