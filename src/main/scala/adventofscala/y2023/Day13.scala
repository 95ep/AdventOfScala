package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

class Day13 extends FileLoader {

  @tailrec
  private def findReflection(
      idx: Int,
      pattern: ArraySeq[ArraySeq[Char]],
      forbidden: Option[Int]
  ): Option[Int] = {
    if (idx == pattern.size) None
    else {
      val firstRaw = pattern.take(idx)
      val secondRaw = pattern.drop(idx)
      val size = firstRaw.size.min(secondRaw.size)
      val first = firstRaw.drop(firstRaw.size - size)
      val second = secondRaw.take(size).reverse
      if (first == second && forbidden.map(_ != idx).getOrElse(true)) {
        Some(idx)
      } else findReflection(idx + 1, pattern, forbidden)
    }
  }

  @tailrec
  private def parsePatterns(
      inputList: List[String],
      patterns: List[ArraySeq[ArraySeq[Char]]],
      currentPattern: ArraySeq[ArraySeq[Char]]
  ): List[ArraySeq[ArraySeq[Char]]] = {
    if (inputList.isEmpty) currentPattern :: patterns
    else {
      val currentLine = inputList.head
      if (currentLine == "") {
        val newPatterns = currentPattern :: patterns
        parsePatterns(inputList.tail, newPatterns, ArraySeq())
      } else {
        val newCurrent =
          currentPattern.appended(ArraySeq.from(inputList.head.toCharArray()))
        parsePatterns(inputList.tail, patterns, newCurrent)
      }
    }
  }

  def scoreForPattern(pattern: ArraySeq[ArraySeq[Char]]): Int = {
    findReflection(1, pattern, None)
      .map(_ * 100)
      .getOrElse(
        findReflection(1, pattern.transpose, None).get
      )
  }

  @tailrec
  private def reflectionInFlip(
      rowIdx: Int,
      colIdx: Int,
      pattern: ArraySeq[ArraySeq[Char]],
      forbidden: Option[Int]
  ): Option[Int] = {
    val nRows = pattern.size
    val nCol = pattern.head.size
    if (colIdx == nCol) { None }
    else {
      val newChar = if (pattern(rowIdx)(colIdx) == '.') '#' else '.'
      val newRow = pattern(rowIdx).updated(colIdx, newChar)
      val newPattern = pattern.updated(rowIdx, newRow)
      val reflectionIdx = findReflection(1, newPattern, forbidden)
      if (reflectionIdx.isDefined) reflectionIdx
      else {
        val newRow = (rowIdx + 1) % nRows
        val newCol = if (newRow == 0) colIdx + 1 else colIdx
        reflectionInFlip(newRow, newCol, pattern, forbidden)
      }
    }
  }

  def scoreForPattern2(pattern: ArraySeq[ArraySeq[Char]]): Int = {
    val horizontalReflection = findReflection(1, pattern, None)
    reflectionInFlip(0, 0, pattern, horizontalReflection)
      .map(_ * 100)
      .getOrElse({

        val verticalReflection = findReflection(1, pattern.transpose, None)
        reflectionInFlip(0, 0, pattern.transpose, verticalReflection).get
      })
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val patterns = parsePatterns(inputList, List(), ArraySeq())

    val answer = patterns.map(scoreForPattern).sum
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val patterns = parsePatterns(inputList, List(), ArraySeq())

    val answer = patterns.map(scoreForPattern2).sum
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
