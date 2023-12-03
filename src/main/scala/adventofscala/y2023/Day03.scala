package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.runtime.stdLibPatches.language.deprecated.symbolLiterals

class Day03 extends FileLoader {
  def findNumbers(
      line: String,
      lineIdx: Int
  ): Iterable[(Int, Int, Int, Int)] = {
    """\d+""".r
      .findAllMatchIn(line)
      .map(m => (m.matched.toInt, m.start, m.end, lineIdx))
      .toList
  }

  def findSymbols(line: String, lineIdx: Int): Iterable[(String, Int, Int)] = {
    """[\D&&[^\.]]""".r
      .findAllMatchIn(line)
      .map(m => (m.matched, m.start, lineIdx))
      .toList
  }

  def matchingSymbol(
      coord: (Int, Int),
      symbols: Iterable[(String, Int, Int)]
  ) = {
    symbols.exists((_, x, y) => x == coord._1 && y == coord._2)
  }

  def matchingNumber(
      coord: (Int, Int),
      numbers: Iterable[(Int, Int, Int, Int)]
  ): Option[(Int, Int, Int, Int)] = {
    numbers.find((_, columnStart, columnEnd, line) =>
      columnStart <= coord._1 && columnEnd > coord._1 && line == coord._2
    )
  }

  def getPossibleNeighbours(
      numberStart: Int,
      numberEnd: Int,
      lineIdx: Int,
      nLines: Int,
      nColumnes: Int
  ): Iterable[(Int, Int)] = {
    val startIdxCol = if (numberStart == 0) numberStart else numberStart - 1
    val endIdxCol = if (numberEnd < nColumnes) numberEnd else numberEnd - 1

    val startIdxLine = if (lineIdx == 0) lineIdx else lineIdx - 1
    val endIdxLine = if (lineIdx == nLines - 1) lineIdx else lineIdx + 1

    Range
      .inclusive(startIdxCol, endIdxCol)
      .flatMap(column =>
        Range.inclusive(startIdxLine, endIdxLine).map(line => (column, line))
      )
  }

  def isPartNumber(
      numberStart: Int,
      numberEnd: Int,
      lineIdx: Int,
      symbols: Iterable[(String, Int, Int)],
      nLines: Int,
      nColumnes: Int
  ): Boolean = {
    val possibleNeighbours =
      getPossibleNeighbours(numberStart, numberEnd, lineIdx, nLines, nColumnes)

    possibleNeighbours.exists(candidate => matchingSymbol(candidate, symbols))
  }

  def getGearRatio(
      symbolIdx: Int,
      line: Int,
      nLines: Int,
      nColumnes: Int,
      partNumbers: Iterable[(Int, Int, Int, Int)]
  ): Int = {
    val adjecentPartNumbers = getPossibleNeighbours(
      symbolIdx,
      symbolIdx + 1,
      line,
      nLines,
      nColumnes
    ).map(neighbourCandidate => matchingNumber(neighbourCandidate, partNumbers))
      .filter(_.isDefined)
      .map(_.get)
      .toList
      .distinct

    println(s"adjecentPartNumbers $adjecentPartNumbers")
    if (adjecentPartNumbers.size == 2) {

      val gearRatio =
        adjecentPartNumbers.map(_._1).product
      println(s"Gear ratio: $gearRatio")
      gearRatio
    } else 0
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList

    val numbers =
      inputList.zipWithIndex.flatMap((line, idx) => findNumbers(line, idx))

    val symbols =
      inputList.zipWithIndex.flatMap((line, idx) => findSymbols(line, idx))

    val answer =
      numbers
        .filter(tup =>
          isPartNumber(
            tup._2,
            tup._3,
            tup._4,
            symbols,
            inputList.size,
            inputList(0).size
          )
        )
        .map(_._1)
        .sum

    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList

    val numbers =
      inputList.zipWithIndex.flatMap((line, idx) => findNumbers(line, idx))

    val symbols =
      inputList.zipWithIndex.flatMap((line, idx) => findSymbols(line, idx))

    val partNumbers =
      numbers
        .filter(tup =>
          isPartNumber(
            tup._2,
            tup._3,
            tup._4,
            symbols,
            inputList.size,
            inputList(0).size
          )
        )

    val asterixSymbols = symbols.filter((s, _, _) => s == "*")
    val answer = asterixSymbols
      .map((_, column, line) =>
        getGearRatio(
          column,
          line,
          inputList.size,
          inputList(0).size,
          partNumbers
        )
      )
      .sum

    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
