package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.collection.mutable.HashMap

class Day08 extends FileLoader {

  class RLInstructions(instructions: String) {
    var idx: Int = 0
    def next: Char = {
      val nextInstruction = instructions(idx)
      idx = (idx + 1) % instructions.size
      nextInstruction
    }
    def reset = idx = 0
  }

  def parseMap(lines: List[String]): HashMap[String, (String, String)] = {
    val map: HashMap[String, (String, String)] = new HashMap()
    val pattern = """(\w{3}) = \((\w{3}), (\w{3})\)""".r
    lines.map(line => {
      val m = pattern.findFirstMatchIn(line).get
      map.addOne((m.group(1), (m.group(2), m.group(3))))
    })
    map
  }

  def walkTheDesert(
      map: HashMap[String, (String, String)],
      instructions: RLInstructions,
      startPosition: String
  ): Long = {
    var doneYet: Boolean = false
    var nSteps: Long = 0
    var currentPos: String = startPosition
    while (!doneYet) {
      nSteps += 1
      val nextTuple = map(currentPos)
      currentPos = if (instructions.next == 'L') nextTuple._1 else nextTuple._2
      doneYet = currentPos.last == 'Z'
    }
    nSteps
  }

  // def walkAsGhost(
  //     map: HashMap[String, (String, String)],
  //     instructions: RLInstructions,
  //     startPositions: List[String]
  // ): Long = {
  //   println(s"Start pos $startPositions")
  //   var doneYet: Boolean = false
  //   var nSteps: Long = 0
  //   var currentPositions: List[String] = startPositions

  //   while (!doneYet) {
  //     nSteps += 1
  //     val nextInstruction = instructions.next
  //     currentPositions = currentPositions.map(pos => {
  //       val nextTuple = map(pos)
  //       if (nextInstruction == 'L') nextTuple._1 else nextTuple._2
  //     })
  //     doneYet = currentPositions.map(pos => pos.last == 'Z').forall(b => b)
  //     // if (currentPositions.map(pos => pos.last == 'Z').exists(b => b)) {
  //     //   println(println(s"Step $nSteps. Current position: $currentPositions"))
  //     // }
  //   }
  //   nSteps
  // }

  def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(1: BigInt) { (a, b) =>
    b * a / Stream
      .iterate((a, b)) { case (x, y) => (y, x % y) }
      .dropWhile(_._2 != 0)
      .head
      ._1
      .abs
  }

  def walkAsGhost(
      map: HashMap[String, (String, String)],
      instructions: RLInstructions,
      startPositions: List[String]
  ): Long = {
    println(s"Start pos $startPositions")
    val stepsList: List[Long] = startPositions.map(pos => {
      instructions.reset
      walkTheDesert(map, instructions, pos)
    })
    println(s"step to reach Z for each start: $stepsList")
    lcm(stepsList.toSeq.map(i => BigInt(i))).toLong
  }

  def part1(inputPath: String): Long = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val instructions = RLInstructions(inputList(0))
    val map = parseMap(inputList.drop(2))

    val answer = walkTheDesert(map, instructions, "AAA")
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Long = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList

    val instructions = RLInstructions(inputList(0))
    val map = parseMap(inputList.drop(2))
    val startPositions: List[String] =
      map.keys.filter(pos => pos.last == 'A').toList
    val answer = walkAsGhost(map, instructions, startPositions)
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
