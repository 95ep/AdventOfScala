package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day15 extends FileLoader {

  @tailrec
  private def hash(chars: Iterable[Char], accumulator: Int): Int = {
    if (chars.isEmpty) accumulator
    else {
      val newAccumulator = ((accumulator + chars.head.toInt) * 17) % 256
      hash(chars.tail, newAccumulator)
    }
  }

  def parseInstruction(s: String): (String, String, Option[Int]) = {
    val m = """(.+)([-=])(\d*)""".r.findFirstMatchIn(s).get
    val i =
      if (m.group(3).nonEmpty) Some(m.group(3).toInt)
      else None
    (m.group(1), m.group(2), i)
  }

  @tailrec
  private def hashmap(
      instr: Iterable[String],
      boxes: Map[Int, List[(String, Int)]]
  ): Map[Int, List[(String, Int)]] = {
    if (instr.isEmpty) boxes
    else {
      val parsedInstructions = parseInstruction(instr.head)
      val lensLabel = parsedInstructions._1
      val boxNr = hash(lensLabel.toCharArray(), 0)
      val newBoxes = if (parsedInstructions._2 == "-") {
        val newContent = boxes(boxNr).filter((lens, _) => lens != lensLabel)
        boxes.updated(boxNr, newContent)
      } else {
        val oldContent = boxes(boxNr)
        val matchingLens =
          oldContent.zipWithIndex.filter((lens, _) => lens._1 == lensLabel)

        val newContent = if (matchingLens.isEmpty) {
          oldContent.appended((lensLabel, parsedInstructions._3.get))
        } else {
          oldContent.updated(
            matchingLens.head._2,
            (lensLabel, parsedInstructions._3.get)
          )
        }
        boxes.updated(boxNr, newContent)
      }
      hashmap(instr.tail, newBoxes)
    }
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList

    val answer =
      inputList.head.split(",").map(s => hash(s.toCharArray(), 0)).sum
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val emptyBoxes = Range(0, 256).map(i => (i, List[(String, Int)]())).toMap
    val finalBoxes = hashmap(inputList.head.split(","), emptyBoxes)

    val answer = finalBoxes
      .flatMap((boxNr, lenses) =>
        lenses.zipWithIndex.map((lens, idx) =>
          (boxNr + 1) * (idx + 1) * lens._2
        )
      )
      .sum

    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
