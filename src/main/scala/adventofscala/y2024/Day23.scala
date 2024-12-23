package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec
import breeze.numerics.exp

class Day23 extends FileLoader {

  @tailrec
  final def parseConnections(
      inputList: List[String],
      connections: Map[String, Set[String]]
  ): Map[String, Set[String]] = {
    if (inputList.isEmpty) connections
    else
      val src = inputList.head.split("-")(0)
      val dst = inputList.head.split("-")(1)
      val dstSet = connections.getOrElse(src, Set())
      val dstSet2 = connections.getOrElse(dst, Set())
      val newConnections =
        connections.updated(src, dstSet + dst).updated(dst, dstSet2 + src)
      parseConnections(inputList.tail, newConnections)
  }

  final def findGroups(
      connections: Map[String, Set[String]],
      root: String
  ): Set[Set[String]] = {
    println(s"Root $root")
    val allDst = connections(root)
    val allGroupCandidates =
      allDst
        .map(c1 => (allDst - c1).map(c2 => Set(c1, c2)))
        .flatten
    val allGroups =
      allGroupCandidates
        .filter(l => connections(l.head).contains(l.tail.head))
        .map(_.take(2).toSet)
        .map(_ + root)

    allGroups

  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val connections = parseConnections(inputList, Map())
    val allComputers = connections.keySet
    val allGroups = allComputers.map(c => findGroups(connections, c)).flatten
    val tGroups = allGroups.filter(l => l.exists(_.startsWith("t")))

    val answer = tGroups.size
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  @tailrec
  final def tryBuildParty(
      expectedSize: Int,
      connections: Map[String, Set[String]],
      candidates: Set[String],
      party: Set[String]
  ): Option[Set[String]] = {
    if (party == Set("co"))
      println()
    if (party.size == expectedSize) Some(party)
    else if (candidates.isEmpty) None
    else {
      val nextCandidate = candidates.head
      val newParty =
        if (
          (connections(nextCandidate) & (connections(
            party.head
          ) + party.head)).size == expectedSize - 1
        ) party + nextCandidate
        else party

      tryBuildParty(expectedSize, connections, candidates.tail, newParty)
    }
  }

  def part2(inputPath: String): String = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val connections = parseConnections(inputList, Map())
    val allComputers = connections.keySet
    val expectedPartySize = connections.map((c, conns) => conns.size).max

    val bestParty = allComputers
      .map(c =>
        tryBuildParty(expectedPartySize, connections, allComputers - c, Set(c))
      )
      .flatten

    val answer = bestParty.head.toList.sorted.mkString(",")
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
