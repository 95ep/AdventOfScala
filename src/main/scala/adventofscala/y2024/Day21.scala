package adventofscala.y2024
import adventofscala.utils.FileLoader

class Day21 extends FileLoader {

  def numericKeypad(from: Char, to: Char): Seq[Char] = {
    val (fRow, fCol) = from match
      case 'a' => (0, 2)
      case '0' => (0, 1)
      case '1' => (1, 0)
      case '2' => (1, 1)
      case '3' => (1, 2)
      case '4' => (2, 0)
      case '5' => (2, 1)
      case '6' => (2, 2)
      case '7' => (3, 0)
      case '8' => (3, 1)
      case '9' => (3, 2)

    val (tRow, tCol) = from match
      case 'a' => (0, 2)
      case '0' => (0, 1)
      case '1' => (1, 0)
      case '2' => (1, 1)
      case '3' => (1, 2)
      case '4' => (2, 0)
      case '5' => (2, 1)
      case '6' => (2, 2)
      case '7' => (3, 0)
      case '8' => (3, 1)
      case '9' => (3, 2)

    val rowDiff = tRow - fRow
    val rowPresses =
      if (rowDiff > 0) List.fill(rowDiff)('^') else List.fill(-rowDiff)('v')

    val colDiff = tCol - fCol
    val colPresses =
      if (colDiff > 0) List.fill(colDiff)('>') else List.fill(-colDiff)('<')

    colPresses ::: rowPresses
  }

  def directionalKeypad(from: Char, to: Char): Seq[Char] = {
    val (fRow, fCol) = from match
      case 'a' => (1, 2)
      case '^' => (1, 1)
      case 'v' => (0, 1)
      case '<' => (0, 0)
      case '>' => (0, 2)

    val (tRow, tCol) = from match
      case 'a' => (0, 2)
      case '^' => (0, 1)
      case 'v' => (1, 0)
      case '<' => (0, 0)
      case '>' => (1, 2)

    val rowDiff = tRow - fRow
    val rowPresses =
      if (rowDiff > 0) List.fill(rowDiff)('^') else List.fill(-rowDiff)('v')

    val colDiff = tCol - fCol
    val colPresses =
      if (colDiff > 0) List.fill(colDiff)('>') else List.fill(-colDiff)('<')

    rowPresses ::: colPresses

  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList

    val answer = 1
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    // val inputList: List[String] = loadLines(inputPath).toList

    val answer = 1
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
