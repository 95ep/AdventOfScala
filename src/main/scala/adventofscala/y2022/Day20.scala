package adventofscala.y2022

import adventofscala.utils.FileLoader
import scala.collection.mutable

class Day20(inputFolder: String) extends Solution2022 with FileLoader {
  override def day(): String = "20"

  val inputPath: String = inputFolder + "/" + fileName

  def mixNTimes(inputList: List[Long], nTimes: Int): List[Long] = {

    val ogDoubleList: List[(Long, Int)] = inputList.zipWithIndex
    val nElements: Int = ogDoubleList.size
    var rearrangedList: List[(Long, Int)] = ogDoubleList

    (1 to nTimes).foreach { _ =>
      ogDoubleList
        .foreach((instruction, idx) => {
          val sourceIdx: Int =
            rearrangedList.indexWhere(element => idx == element._2)
          val unwarpedDestIdx: Int =
            sourceIdx + (instruction % (nElements - 1)).toInt

          val destIdx: Int =
            if (unwarpedDestIdx <= 0) nElements - 1 + unwarpedDestIdx
            else if (unwarpedDestIdx >= nElements)
              unwarpedDestIdx - (nElements - 1)
            else unwarpedDestIdx

          val tmpList: List[(Long, Int)] =
            rearrangedList.slice(0, sourceIdx) ::: rearrangedList.slice(
              sourceIdx + 1,
              nElements
            )
          rearrangedList =
            tmpList.slice(0, destIdx) ::: (instruction, idx) :: tmpList
              .slice(destIdx, nElements)
        })
    }
    rearrangedList.map(_._1)
  }

  def getCoordsSum(decryptedList: List[Long]): Long = {
    val idxOfZero: Int = decryptedList.indexOf(0)
    List(1000, 2000, 3000)
      .map(offset => (idxOfZero + offset) % decryptedList.size)
      .map(decryptedList(_))
      .sum
  }

  def part1: Long = {
    println("Running part 1")
    val inputList: List[Long] =
      loadLines(inputPath).toList.map(l => l.toLong)
    val decryptedList: List[Long] = mixNTimes(inputList, 1)
    val answer = getCoordsSum(decryptedList)
    println(s"Answer is $answer")
    answer
  }

  def part2: Long = {
    println("Running part 2")
    val decryptionKey: Long = 811589153L
    val inputList: List[Long] =
      loadLines(inputPath).toList.map(l => l.toLong * decryptionKey)
    val decryptedList: List[Long] = mixNTimes(inputList, 10)
    val answer = getCoordsSum(decryptedList)
    println(s"Answer is $answer")
    answer
  }
}
