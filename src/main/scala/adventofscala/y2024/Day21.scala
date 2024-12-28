package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day21 extends FileLoader {

  val numericKeypadMapping: Map[Char, (Int, Int)] =
    Map(
      'A' -> (0, 2),
      '0' -> (0, 1),
      '1' -> (1, 0),
      '2' -> (1, 1),
      '3' -> (1, 2),
      '4' -> (2, 0),
      '5' -> (2, 1),
      '6' -> (2, 2),
      '7' -> (3, 0),
      '8' -> (3, 1),
      '9' -> (3, 2)
    )

  def numericKeypadToFrom(to: Char, from: Char): Seq[String] = {
    val toPos = numericKeypadMapping(to)
    val fromPos = numericKeypadMapping(from)
    val horizontalDirection = if (toPos._2 < fromPos._2) '<' else '>'
    val horizontalMove =
      List.fill((toPos._2 - fromPos._2).abs)(horizontalDirection)
    val verticalDirection = if (toPos._1 < fromPos._1) 'v' else '^'
    val verticalMove = List.fill((toPos._1 - fromPos._1).abs)(verticalDirection)
    if (toPos._1 == 0 && fromPos._2 == 0)
      List(horizontalMove ::: verticalMove).map(_.mkString)
    else if (fromPos._1 == 0 && toPos._2 == 0)
      List(verticalMove ::: horizontalMove).map(_.mkString)
    else {
      List(horizontalMove ::: verticalMove, verticalMove ::: horizontalMove)
        .map(_.mkString)
    }
  }

  val directionalKeypadPaths: Map[Char, Map[Char, Seq[String]]] = {
    Map(
      // 'A': {"A": [""], "^": ["<"], ">": ["v"], "v": ["<v", "v<"], "<": ["<v<", "v<<"]},
      'A' -> Map(
        'A' -> Seq(""),
        '^' -> Seq("<"),
        'v' -> Seq(
          "<v",
          "v<"
        ),
        '>' -> Seq("v"),
        '<' -> Seq(
          "v<<"
          //  "<v<"
        )
      ),
      // '^': {"^": [""], "A": [">"], "v": ["v"], "<": ["v<"], ">": ["v>"]},
      '^' -> Map(
        'A' -> Seq(">"),
        '^' -> Seq(""),
        'v' -> Seq("v"),
        '>' -> Seq(
          ">v",
          "v>"
        ),
        '<' -> Seq("v<")
      ),
      // '>': {">": [""], "A": ["^"], "^": ["^<", "<^"], "v": ["<"], "<": ["<<"]},
      '>' -> Map(
        'A' -> Seq("^"),
        '^' -> Seq(
          "^<",
          "<^"
        ),
        'v' -> Seq("<"),
        '>' -> Seq(""),
        '<' -> Seq("<<")
      ),
      // 'v': {"v": [""], "A": ["^>", ">^"], "^": ["^"], "<": ["<"], ">": [">"]},
      'v' -> Map(
        'A' -> Seq(
          "^>",
          ">^"
        ),
        '^' -> Seq(
          "^"
          // ">^<"
        ),
        'v' -> Seq(""),
        '>' -> Seq(
          ">"
          //  "^>v"
        ),
        '<' -> Seq("<")
      ),
      // '<': {"<": [""], "A": [">>^", ">^>"], "^": [">^"], "v": [">"], ">": [">>"]},
      '<' -> Map(
        'A' -> Seq(
          ">>^"
          // ">^>"
        ),
        '^' -> Seq(
          ">^"
          // ">>^<"
        ),
        'v' -> Seq(">"),
        '>' -> Seq(">>"),
        '<' -> Seq("")
      )
    )
  }

  @tailrec
  final def allPathsNumericAsClicks(
      from: Char,
      code: String,
      paths: Set[String]
  ): Set[String] = {
    if (code.isEmpty) paths
    else {
      val to = code.head
      val segmentPaths = numericKeypadToFrom(to, from)
      val newPaths =
        paths
          .map(oldPaths => segmentPaths.map(newSeg => oldPaths + newSeg + 'A'))
          .flatten
      allPathsNumericAsClicks(to, code.tail, newPaths)
    }
  }

  val initialCache = {
    val allKeys = Seq('A', '<', '>', '^', 'v')
    (for
      c1 <- allKeys
      c2 <- allKeys
    yield Seq(c1, c2).mkString -> (directionalKeypadPaths(c1)(
      c2
    ).head + 'A')).toMap

  }

  def shortestPathCode(code: String, nRobots: Int): Long = {
    val clickSeqsOnKeypad1 =
      allPathsNumericAsClicks('A', code, Set("A")).toList
    val allKeys = Seq('A', '<', '>', '^', 'v')
    val allKeyCombs = for
      c1 <- allKeys
      c2 <- allKeys
    yield Seq(c1, c2).mkString
    val humanCosts = allKeyCombs.map(k => k -> 1L).toMap
    val costs = Range(0, nRobots).foldLeft(humanCosts)((costs, layer) => {
      val newCosts = costs.keys
        .map(keyComb =>
          val pathsOnPad = directionalKeypadPaths(keyComb.head)(keyComb.last)
            .map(p => 'A' + p + 'A')
          val costsOnPad = pathsOnPad.map(path => {
            val subMoves = Range(0, path.size - 1).map(j =>
              Seq(path(j), path(j + 1)).mkString
            )
            subMoves.map(costs(_)).sum
          })
          keyComb -> costsOnPad.min
        )
        .toMap
      println(s"Founds costs for keypad $layer")
      newCosts
    })

    val costPerPath = clickSeqsOnKeypad1.map(p => {
      Range(0, p.size - 1)
        .map(i => costs(Seq(p(i), p(i + 1)).mkString))
        .sum
    })
    costPerPath.min
  }

  def complexityScore(code: String, nRobots: Int): Long = {
    val shortestPathCost = shortestPathCode(code, nRobots)
    val numericPart = code.dropRight(1).toInt
    shortestPathCost * numericPart
  }

  def part1(inputPath: String): Long = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val scores = inputList.map(complexityScore(_, 2))

    val answer = scores.sum
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Long = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val scores = inputList.map(complexityScore(_, 25))

    val answer = scores.sum
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
