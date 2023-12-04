package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.collection.mutable.HashMap

class Day04 extends FileLoader {

  case class ScratchCard(cardNo: Int, winning: List[Int], yours: List[Int]) {
    def calcScore: Int = {
      Math.pow(2, calcNWinns - 1).toInt
    }

    def calcNWinns: Int = { yours.filter(n => winning.contains(n)).size }
  }

  def parseCard(line: String): ScratchCard = {
    val cardNoSplit = line.split(":")
    val cardNo: Int =
      """Card +(\d+)""".r.findFirstMatchIn(cardNoSplit.head).get.group(1).toInt

    val winningSplit = cardNoSplit(1).split("\\|")
    val winning: List[Int] =
      winningSplit.head
        .split(" ")
        .map(e => e.toIntOption)
        .filter(op => op.isDefined)
        .map(op => op.get)
        .toList

    val yours: List[Int] =
      winningSplit(1)
        .split(" ")
        .map(e => e.toIntOption)
        .filter(op => op.isDefined)
        .map(op => op.get)
        .toList

    ScratchCard(cardNo, winning, yours)
  }
  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList

    val answer = inputList.map(parseCard).map(card => card.calcScore).sum

    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList

    val parsedCards = inputList.map(parseCard)

    val cardMap: HashMap[Int, Int] = HashMap()
    val allNumbers = parsedCards.map(card => cardMap.addOne(card.cardNo, 1))
    parsedCards.map(card => {
      val nWinns = card.calcNWinns
      if (nWinns > 0) {
        val wonIdx: Range = Range(card.cardNo + 1, card.cardNo + 1 + nWinns)
        wonIdx.map(idx =>
          cardMap.updateWith(idx)(opInt =>
            opInt.map(i => i + cardMap(card.cardNo))
          )
        )
      }
    })

    val answer = cardMap.values.sum

    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
