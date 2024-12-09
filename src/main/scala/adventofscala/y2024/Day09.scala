package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec
import breeze.numerics.I

class Day09 extends FileLoader {

  @tailrec
  private def compact(
      memMap: Vector[Int],
      emptyPos: Int,
      filePos: Int
  ): Seq[Int] = {
    if (emptyPos == filePos) memMap
    else {
      if (memMap(emptyPos) != -1) { compact(memMap, emptyPos + 1, filePos) }
      else if (memMap(filePos) == -1) { compact(memMap, emptyPos, filePos - 1) }
      else {
        val newMemMap =
          memMap.take(emptyPos)
            ++: memMap(filePos)
            +: memMap.slice(
              emptyPos + 1,
              filePos
            )
            ++: -1 +: memMap.takeRight(memMap.size - filePos - 1)
        compact(newMemMap, emptyPos + 1, filePos - 1)
      }
    }
  }

  case class File(idx: Int, size: Int, id: Int) {
    def moveTo(newIdx: Int) = File(newIdx, size, id)
    def checkSum: Long = Range(idx, idx + size).map(_ * id.toLong).sum
  }

  @tailrec
  final def parseMemory(
      memory: Seq[Char],
      strIdx: Int,
      memIdx: Int,
      files: List[File],
      emptySpace: Vector[Int]
  ): (List[File], Vector[Int]) = {
    if (memory.isEmpty) (files, emptySpace)
    else {
      val blockSize = memory.head.asDigit
      if (strIdx % 2 == 0) {
        val newFiles = files :+ File(memIdx, blockSize, strIdx / 2)
        parseMemory(
          memory.tail,
          strIdx + 1,
          memIdx + blockSize,
          newFiles,
          emptySpace
        )
      } else {
        val newEmptySpace =
          emptySpace ++: Range(memIdx, memIdx + memory.head.asDigit).toVector
        parseMemory(
          memory.tail,
          strIdx + 1,
          memIdx + blockSize,
          files,
          newEmptySpace
        )
      }
    }
  }

  @tailrec
  final def findEmptySpace(
      emptySpace: Vector[Int],
      size: Int
  ): Option[Int] = {
    if (emptySpace.size < size) None
    else {
      val idx = emptySpace.head
      if (emptySpace.take(size) == Range(idx, idx + size)) Some(idx)
      else { findEmptySpace(emptySpace.tail, size) }
    }
  }

  @tailrec
  final def compactMemory(
      files: List[File],
      emptySpace: Vector[Int],
      compactedMemory: List[File]
  ): List[File] = {
    if (files.isEmpty) { compactedMemory }
    else {
      val file = files.head
      val truncatedEmptySpace = emptySpace.filter(_ < file.idx)
      val emptySpaceIdx = findEmptySpace(truncatedEmptySpace, file.size)
      if (emptySpaceIdx.isDefined) {
        val movedFile = file.moveTo(emptySpaceIdx.get)
        val removedIndicies =
          Range(emptySpaceIdx.get, emptySpaceIdx.get + file.size).toSet
        val updateEmptySpace =
          truncatedEmptySpace.filter(!removedIndicies.contains(_))
        val updatedCompactedMemory = compactedMemory :+ movedFile
        compactMemory(files.tail, updateEmptySpace, updatedCompactedMemory)
      } else {
        val updatedCompactedMemory = compactedMemory :+ file
        compactMemory(files.tail, truncatedEmptySpace, updatedCompactedMemory)
      }
    }
  }

  def part1(inputPath: String): Long = {
    println("Running part 1")
    val inputLine: String = loadLines(inputPath).toList.head
    val files =
      for i <- 0 until inputLine.size / 2 + 1
      yield (inputLine(i * 2).asDigit)
    val emptySpace =
      for i <- 0 until inputLine.size / 2
      yield inputLine(i * 2 + 1).asDigit

    val memMap =
      (for i <- 0 until emptySpace.size
      yield Vector.fill(files(i))(i) ++: Vector.fill(emptySpace(i))(
        -1
      )).flatten ++: Vector.fill(files(emptySpace.size))(emptySpace.size)

    val compacted = compact(memMap, 0, memMap.size - 1)

    val answer = (for i <- 0 until compacted.size if compacted(i) != -1
    yield i * compacted(i).toLong).sum
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Long = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val (files, emptySpace) =
      parseMemory(inputList.head.toList, 0, 0, List(), Vector())

    val compacted = compactMemory(files.reverse, emptySpace, List())
    val answer = compacted.map(_.checkSum).sum
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
