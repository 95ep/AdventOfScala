package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day18 extends FileLoader {
  def parseDigInstruction1(input: String): (String, Long) = {
    val splitted = input.split(" ")
    (splitted(0), splitted(1).toLong)
  }

  def parseDigInstruction2(input: String): (String, Long) = {
    val splitted = input.split(" ")
    val cleanHex = splitted(2).drop(2).dropRight(1)
    val dir: String = cleanHex.last match
      case '0' => "R"
      case '1' => "D"
      case '2' => "L"
      case '3' => "U"

    val dist = java.lang.Long.parseLong(cleanHex.dropRight(1), 16)
    (dir, dist)
  }

  def shoelace(poly: List[(Long, Long)]): Long = {
    val polySize = poly.size
    val accum = Range(0, polySize).foldLeft((0L, 0L))((accum, i) => {
      val plusOne = (i + 1) % polySize
      val accumPos = accum._1 + poly(i)._1 * poly(plusOne)._2
      val accumNeg = accum._2 + poly(i)._2 * poly(plusOne)._1
      (accumPos, accumNeg)
    })
    ((accum._1 - accum._2) / 2).abs
  }

  def circumference(poly: List[(Long, Long)]): Long = {
    val adjusted = poly.drop(1) :+ poly.head
    poly
      .zip(adjusted)
      .foldLeft(0L)((accum, p) =>
        accum + (p._1._1 - p._2._1).abs + (p._1._2 - p._2._2).abs
      )
  }

  @tailrec
  private def getPolygon(
      poly: List[(Long, Long)],
      digInst: List[(String, Long)]
  ): List[(Long, Long)] = {
    if (digInst.isEmpty) poly
    else {
      val currentPoint = poly.headOption.getOrElse(0L, 0L)
      val newPoint = digInst.head match
        case (dir, dist) if dir == "R" =>
          (currentPoint._1, currentPoint._2 + dist)
        case (dir, dist) if dir == "L" =>
          (currentPoint._1, currentPoint._2 - dist)
        case (dir, dist) if dir == "D" =>
          (currentPoint._1 + dist, currentPoint._2)
        case (dir, dist) if dir == "U" =>
          (currentPoint._1 - dist, currentPoint._2)

      getPolygon(newPoint :: poly, digInst.tail)
    }
  }

  def solution(digInstructions: List[(String, Long)]): Long = {
    val poly = getPolygon(List(), digInstructions)
    val area = shoelace(poly)
    val cir = circumference(poly)

    area + cir / 2 + 1
  }

  def part1(inputPath: String): Long = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val digInstructions = inputList.map(parseDigInstruction1)

    val answer = solution(digInstructions)
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Long = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val digInstructions = inputList.map(parseDigInstruction2)

    val answer = solution(digInstructions)
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
