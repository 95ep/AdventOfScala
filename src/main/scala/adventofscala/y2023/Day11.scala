package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day11 extends FileLoader {

  def findGalaxies(image: Array[Array[Char]]): Array[(Int, Int)] = {
    image.zipWithIndex
      .flatMap((row, rIdx) => {
        row.zipWithIndex.map((c, cIdx) =>
          if (c == '#') { (rIdx, cIdx) }
          else { (-1, -1) }
        )
      })
      .filter(_ != (-1, -1))
  }

  def soution(inputList: List[String], spaceFactor: Int): Long = {
    val image: Array[Array[Char]] = inputList.map(_.toCharArray).toArray
    val rowsToExpand: Array[Int] = image.zipWithIndex
      .map((row, rIdx) => {
        if (row.forall(c => c == '.')) { rIdx }
        else { -1 }
      })
      .filter(_ > -1)

    val nColumns = image.head.size
    val nRows = image.size
    val columnsToExpand = Range(0, nColumns)
      .map(c =>
        val column = Range(0, nRows).map(r => image(r)(c))
        (if (column.forall(c => c == '.')) { c }
         else { -1 })
      )
      .filter(_ > -1)

    val galaxies = findGalaxies(image)
    galaxies
      .combinations(2)
      .map(a => {
        val galaxyA = a(0)
        val galaxyB = a(1)
        List(galaxyA._1, galaxyB._1).min
        val verticalRange = Range(
          List(galaxyA._1, galaxyB._1).min,
          List(galaxyA._1, galaxyB._1).max + 1
        )
        val horizontalRange = Range(
          List(galaxyA._2, galaxyB._2).min,
          List(galaxyA._2, galaxyB._2).max + 1
        )

        val vertDist: Long =
          verticalRange.last - verticalRange.head + rowsToExpand
            .count(rIdx => verticalRange.contains(rIdx)) * (spaceFactor - 1)

        val horizDist: Long =
          horizontalRange.last - horizontalRange.head + columnsToExpand
            .count(cIdx => horizontalRange.contains(cIdx)) * (spaceFactor - 1)

        horizDist + vertDist
      })
      .sum
  }

  def part1(inputPath: String): Long = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList

    val answer = soution(inputList, 2)
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String, spaceFactor: Int): Long = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val answer = soution(inputList, spaceFactor)
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
