package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day19 extends FileLoader {

  def splitCategory(
      cat: (Int, Int),
      limit: Int,
      lowerBound: Boolean
  ): (Option[(Int, Int)], Option[(Int, Int)]) = {
    cat match
      case (a, b) if lowerBound && a > limit => (Some(cat), None)
      case (a, b) if lowerBound && a <= limit && b > limit =>
        (Some(limit + 1, b), Some(a, limit))
      case (a, b) if lowerBound && b <= limit => (None, Some(a, b))
      case (a, b) if !lowerBound && b < limit => (Some(a, b), None)
      case (a, b) if !lowerBound && b >= limit && a < limit =>
        (Some(a, limit - 1), Some(limit, b))
      case (a, b) if !lowerBound && a >= limit => (None, Some(a, b))
  }

  def splitRange(
      range: ((Int, Int), (Int, Int), (Int, Int), (Int, Int)),
      filter: (Int, Int, Boolean, String)
  ): (
      Option[((Int, Int), (Int, Int), (Int, Int), (Int, Int))],
      Option[((Int, Int), (Int, Int), (Int, Int), (Int, Int))]
  ) = {
    filter._1 match
      case 1 =>
        (
          splitCategory(range._1, filter._2, filter._3)._1
            .map((_, range._2, range._3, range._4)),
          splitCategory(range._1, filter._2, filter._3)._2
            .map((_, range._2, range._3, range._4))
        )
      case 2 =>
        (
          splitCategory(range._2, filter._2, filter._3)._1
            .map((range._1, _, range._3, range._4)),
          splitCategory(range._2, filter._2, filter._3)._2
            .map((range._1, _, range._3, range._4))
        )
      case 3 =>
        (
          splitCategory(range._3, filter._2, filter._3)._1
            .map((range._1, range._2, _, range._4)),
          splitCategory(range._3, filter._2, filter._3)._2
            .map((range._1, range._2, _, range._4))
        )
      case 4 =>
        (
          splitCategory(range._4, filter._2, filter._3)._1
            .map((range._1, range._2, range._3, _)),
          splitCategory(range._4, filter._2, filter._3)._2
            .map((range._1, range._2, range._3, _))
        )
  }

  def parseFilter(s: String): (Int, Int, Boolean, String) = {
    """(\D)([<>])(\d+):(\D+)""".r
      .findFirstMatchIn(s)
      .map(m => {
        val lowerBound = m.group(2) == ">"
        val limit = m.group(3).toInt
        val dest = m.group(4)
        val resourceIdx = m.group(1) match
          case "x" => 1
          case "m" =>
            2
          case "a" => 3
          case "s" => 4

        (resourceIdx, limit, lowerBound, dest)
      })
      .get
  }

  def parseZeroFilters(s: String): List[(Int, Int, Boolean, String)] = List(
    (1, 0, true, s),
    (2, 0, true, s),
    (3, 0, true, s),
    (4, 0, true, s)
  )

  @tailrec
  private def parseInput(
      inputList: List[String],
      workflows: Map[String, List[(Int, Int, Boolean, String)]],
      parts: List[(Int, Int, Int, Int)]
  ): (
      Map[String, List[(Int, Int, Boolean, String)]],
      List[(Int, Int, Int, Int)]
  ) = {
    if (inputList.isEmpty) (workflows, parts)
    else {
      val line = inputList.head
      val newWorkflows = """(.+)\{(.+)\}""".r
        .findFirstMatchIn(line)
        .map(m => {

          val rawFilters = m.group(2).split(",")
          val filters =
            rawFilters.dropRight(1).map(parseFilter).toList ++ parseZeroFilters(
              rawFilters.last
            )
          workflows.updated(m.group(1), filters)
        })

      val newParts = """\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}""".r
        .findFirstMatchIn(line)
        .map(m =>
          (
            m.group(1).toInt,
            m.group(2).toInt,
            m.group(3).toInt,
            m.group(4).toInt
          ) :: parts
        )

      parseInput(
        inputList.tail,
        newWorkflows.getOrElse(workflows),
        newParts.getOrElse(parts)
      )
    }
  }

  @tailrec
  private def getAcceptedRanges(
      rangesWithWf: Set[
        (
            String,
            ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
        )
      ],
      accepted: Set[((Int, Int), (Int, Int), (Int, Int), (Int, Int))],
      workflows: Map[String, List[(Int, Int, Boolean, String)]]
  ): Set[((Int, Int), (Int, Int), (Int, Int), (Int, Int))] = {
    if (rangesWithWf.isEmpty) accepted
    else {
      val next = rangesWithWf.head
      val nextWfKey = next._1
      val nextRange = next._2
      val newRanges = workflows(nextWfKey)
        .foldLeft(
          (
            Some(nextRange): Option[
              ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
            ],
            List[(String, ((Int, Int), (Int, Int), (Int, Int), (Int, Int)))]()
          )
        )((accum, filter) => {
          val n = accum._1
          val r = accum._2
          if (n.isDefined) {
            val s = splitRange(n.get, filter)
            val rr = s._1.map(rrr => r.appended((filter._4, rrr))).getOrElse(r)
            val outside = s._2
            (outside, rr)

          } else (n, r)
        })
        ._2

      val newAccepted = newRanges.filter((d, _) => d == "A").map((_, r) => r)
      val newRangesWithWf = newRanges.filter((d, _) => d != "A" && d != "R")

      getAcceptedRanges(
        rangesWithWf ++ newRangesWithWf - next,
        accepted ++ newAccepted,
        workflows
      )
    }
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList

    val parsed = parseInput(inputList, Map(), List())
    val workflows = parsed._1
    val parts = parsed._2
      .map(t =>
        ("in", ((t._1, t._1), (t._2, t._2), (t._3, t._3), (t._4, t._4)))
      )
      .toSet
    val acceptedRanges = getAcceptedRanges(parts, Set(), workflows)
    val answer2 =
      acceptedRanges.map(r => r._1._1 + r._2._1 + r._3._1 + r._4._1).sum

    println(s"${this.getClass()}: The answer to part one is $answer2")
    answer2
  }

  def part2(inputPath: String): Long = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val workflows = parseInput(inputList, Map(), List())._1
    val acceptedRanges =
      getAcceptedRanges(
        Set(("in", ((1, 4000), (1, 4000), (1, 4000), (1, 4000)))),
        Set(),
        workflows
      )

    val answer = acceptedRanges.toList
      .map(r =>
        (r._1._2 - r._1._1 + 1L) * (r._2._2 - r._2._1 + 1L) * (r._3._2 - r._3._1 + 1L) * (r._4._2 - r._4._1 + 1L)
      )
      .sum

    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
