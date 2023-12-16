package adventofscala.y2023
import adventofscala.utils.FileLoader

class Day16 extends FileLoader {

  def findNextPos(
      pos: (Int, Int),
      direction: Char,
      tile: Char
  ): List[((Int, Int), Char)] = {
    tile match
      case '.' =>
        direction match
          case '^' => List(((pos._1 - 1, pos._2), '^'))
          case '>' => List(((pos._1, pos._2 + 1), '>'))
          case 'v' => List(((pos._1 + 1, pos._2), 'v'))
          case '<' => List(((pos._1, pos._2 - 1), '<'))
      case '/' =>
        direction match
          case '^' => List(((pos._1, pos._2 + 1), '>'))
          case '>' => List(((pos._1 - 1, pos._2), '^'))
          case 'v' => List(((pos._1, pos._2 - 1), '<'))
          case '<' => List(((pos._1 + 1, pos._2), 'v'))

      case '\\' =>
        direction match
          case '^' => List(((pos._1, pos._2 - 1), '<'))
          case '>' => List(((pos._1 + 1, pos._2), 'v'))
          case 'v' => List(((pos._1, pos._2 + 1), '>'))
          case '<' => List(((pos._1 - 1, pos._2), '^'))

      case '|' =>
        direction match
          case '^' => List(((pos._1 - 1, pos._2), '^'))
          case '>' =>
            List(((pos._1 + 1, pos._2), 'v'), ((pos._1 - 1, pos._2), '^'))
          case 'v' => List(((pos._1 + 1, pos._2), 'v'))
          case '<' =>
            List(((pos._1 + 1, pos._2), 'v'), ((pos._1 - 1, pos._2), '^'))

      case '-' =>
        direction match
          case '^' =>
            List(((pos._1, pos._2 + 1), '>'), ((pos._1, pos._2 - 1), '<'))
          case '>' => List(((pos._1, pos._2 + 1), '>'))
          case 'v' =>
            List(((pos._1, pos._2 + 1), '>'), ((pos._1, pos._2 - 1), '<'))
          case '<' => List(((pos._1, pos._2 - 1), '<'))

  }

  private def rayTracing(
      currentBeams: List[((Int, Int), Char)],
      contraption: List[List[Char]],
      visited: Map[(Int, Int), List[Char]]
  ): Map[(Int, Int), List[Char]] = {
    if (currentBeams.isEmpty) visited
    else {
      val pos = currentBeams.head._1
      val direction = currentBeams.head._2

      val updated =
        if (
          -1 < pos._1 && pos._1 < contraption.size && -1 < pos._2 && pos._2 < contraption.head.size && visited
            .get(pos)
            .map(!_.contains(direction))
            .getOrElse(true)
        ) {
          val v = visited.get(pos).getOrElse(List()).appended(direction)
          val newVisited = visited.updated(pos, v)
          val next =
            findNextPos(pos, direction, contraption(pos._1)(pos._2))
          val newCurrentBeams = currentBeams.tail ::: next
          (newCurrentBeams, newVisited)
        } else (currentBeams.tail, visited)

      rayTracing(updated._1, contraption, updated._2)
    }
  }

  def testAllStartPos(contraption: List[List[Char]]): Int = {
    val fromLeft = Range(0, contraption.size)
      .map(i => rayTracing(List(((i, 0), '>')), contraption, Map()))

    val fromRight = Range(0, contraption.size).map(i =>
      rayTracing(
        List(((i, contraption.head.size - 1), '<')),
        contraption,
        Map()
      )
    )

    val fromUp = Range(0, contraption.head.size).map(i =>
      rayTracing(
        List(((0, i), 'v')),
        contraption,
        Map()
      )
    )

    val fromDown = Range(0, contraption.head.size).map(i =>
      rayTracing(
        List(((contraption.size - 1, i), '^')),
        contraption,
        Map()
      )
    )

    (fromLeft ++ fromRight ++ fromUp ++ fromDown).map(_.size).max
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[List[Char]] = loadLines(inputPath).toList.map(_.toList)

    val answer = rayTracing(List(((0, 0), '>')), inputList, Map()).size
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val contraption: List[List[Char]] =
      loadLines(inputPath).toList.map(_.toList)

    val answer = testAllStartPos(contraption)
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
