package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

class Day14 extends FileLoader {
  def parsePlatform(input: List[String]): ArraySeq[ArraySeq[Char]] = {
    ArraySeq.from(input.map(s => ArraySeq.from(s.toCharArray())))
  }

  @tailrec
  private def findNewPos(
      platform: ArraySeq[ArraySeq[Char]],
      rowIdx: Int,
      columnIdx: Int,
      rowDelta: Int,
      columnDelta: Int
  ): (Int, Int) = {
    val nRows = platform.size
    val nCol = platform.head.size
    val newRow = rowIdx - rowDelta
    val newCol = columnIdx - columnDelta

    if (
      newRow == nRows || newRow < 0 || newCol == nCol || newCol < 0 || platform(
        newRow
      )(newCol) != '.'
    )
      (rowIdx, columnIdx)
    else findNewPos(platform, newRow, newCol, rowDelta, columnDelta)

  }

  @tailrec
  private def moveStones(
      platform: ArraySeq[ArraySeq[Char]],
      rowIdx: Int,
      columnIdx: Int,
      rowDelta: Int,
      columnDelta: Int
  ): ArraySeq[ArraySeq[Char]] = {
    val nRows = platform.size
    val nCol = platform.head.size
    if (nCol == columnIdx || columnIdx < 0 || rowIdx == nRows || rowIdx < 0)
      platform
    else {
      val currentElement = platform(rowIdx)(columnIdx)
      val newPlatform = if (currentElement == 'O') {
        val newPos =
          findNewPos(platform, rowIdx, columnIdx, rowDelta, columnDelta)

        val replaceStone =
          platform.updated(rowIdx, platform(rowIdx).updated(columnIdx, '.'))
        replaceStone.updated(
          newPos._1,
          replaceStone(newPos._1).updated(newPos._2, 'O')
        )

      } else platform

      val newRow = rowIdx + rowDelta
      val newCol = columnIdx + columnDelta

      val adjustedIdxs = if (rowDelta != 0) {
        if (newRow == nRows) { (0, newCol + 1) }
        else if (newRow < 0) (nRows - 1, newCol + 1)
        else (newRow, newCol)

      } else {
        if (newCol == nCol) { (newRow + 1, 0) }
        else if (newCol < 0) (newRow + 1, nCol - 1)
        else (newRow, newCol)
      }

      moveStones(
        newPlatform,
        adjustedIdxs._1,
        adjustedIdxs._2,
        rowDelta,
        columnDelta
      )
    }
  }

  @tailrec
  private def cycleTilts(
      platform: ArraySeq[ArraySeq[Char]],
      prevConfigs: List[ArraySeq[ArraySeq[Char]]],
      nCycles: Long
  ): Int = {

    val tiltedNorth = moveStones(platform, 0, 0, 1, 0)
    val tiltedWest = moveStones(tiltedNorth, 0, 0, 0, 1)
    val tiltedSouth = moveStones(tiltedWest, platform.size - 1, 0, -1, 0)
    val tiltedEast = moveStones(tiltedSouth, 0, platform.head.size - 1, 0, -1)
    if (prevConfigs.contains(tiltedEast)) {
      val offset = prevConfigs.indexOf(tiltedEast)
      val cycleLength =
        prevConfigs.size - offset
      val theCycle = prevConfigs.drop(offset)
      val finalConfig =
        theCycle(((nCycles - offset - 1) % cycleLength).toInt)
      calcWeight(finalConfig)
    } else {
      val updatedprevConfigs = prevConfigs.appended(tiltedEast)
      cycleTilts(tiltedEast, updatedprevConfigs, nCycles)
    }

  }

  def calcWeight(platform: ArraySeq[ArraySeq[Char]]): Int = {
    platform.reverse.zipWithIndex
      .map((row, i) => row.count(_ == 'O') * (i + 1))
      .sum
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val platform = parsePlatform(inputList)
    val newPlatform = moveStones(platform, 0, 0, 1, 0)
    val answer = calcWeight(newPlatform)
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")

    val nCycles: Long = 1000000000L
    val inputList: List[String] = loadLines(inputPath).toList
    val platform = parsePlatform(inputList)
    val answer = cycleTilts(platform, List(), nCycles)

    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
