package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec
import java.lang.Long

class Day24 extends FileLoader {

  case class Gate(inp1: String, inp2: String, out: String, op: String) {
    def getOutput(in1: Boolean, in2: Boolean): Boolean = {
      op match
        case "AND" => in1 && in2
        case "OR"  => in1 || in2
        case "XOR" => in1 ^ in2

    }
  }

  def wireValues(inputList: List[String]): Map[String, Boolean] = {
    val nInitialValues = inputList.indexOf("")
    inputList
      .take(nInitialValues)
      .foldLeft(Map[String, Boolean]())((m, l) => {
        val wire = l.split(":").head
        m.updated(wire, l.last == '1')
      })
  }

  def getGates(inputList: List[String]): List[Gate] = {
    val nInitialValues = inputList.indexOf("")
    val pattern = raw"(\w+) (\w+) (\w+) -> (\w+)".r

    inputList
      .drop(nInitialValues + 1)
      .map(l => {
        val m = pattern.findFirstMatchIn(l).get
        Gate(m.group(1), m.group(3), m.group(4), m.group(2))
      })
  }

  @tailrec
  final def runGates(
      gates: List[Gate],
      wires: Map[String, Boolean]
  ): Map[String, Boolean] = {
    if (gates.isEmpty) wires
    else {
      val gate = gates.head
      if (wires.contains(gate.inp1) && wires.contains(gate.inp2)) {
        val output = gate.getOutput(wires(gate.inp1), wires(gate.inp2))
        val newWires = wires.updated(gate.out, output)
        runGates(gates.tail, newWires)
      } else {
        runGates(gates.tail :+ gate, wires)
      }
    }
  }

  def parseNumber(wires: Map[String, Boolean], wireKey: Char): Long = {
    val binaryString =
      wires
        .filter((k, _) => k.startsWith(wireKey.toString()))
        .toList
        .sortBy((k, _) => k)
        .reverse
        .map(_._2)
        .map(b => if (b) 1 else 0)
        .mkString
    Long.parseLong(binaryString, 2)
  }

  def part1(inputPath: String): Long = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val wires = wireValues(inputList)
    val gates = getGates(inputList)

    val newWires = runGates(gates, wires)
    val binaryString =
      newWires
        .filter((k, _) => k.startsWith("z"))
        .toList
        .sortBy((k, _) => k)
        .reverse
        .map(_._2)
        .map(b => if (b) 1 else 0)
        .mkString

    val answer = parseNumber(newWires, 'z')
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def findCorrespondingZ(
      stackedGates: List[Gate],
      allGates: List[Gate]
  ): String = {
    val nextGate = stackedGates.head
    if (nextGate.out.startsWith("z"))
      "z" + "%02d".format(nextGate.out.drop(1).toInt - 1)
    else
      findCorrespondingZ(
        stackedGates.tail ++ allGates.filter(g => {
          g.inp1 == nextGate.out || g.inp2 == nextGate.out
        }),
        allGates
      )
  }

  def part2(inputPath: String): String = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val wires = wireValues(inputList)
    val gates = getGates(inputList)

    val zGates =
      gates.filter(g => g.out.startsWith("z")).filterNot(g => g.out == "z45")

    val breakingRule1 = zGates.filter(g => g.op != "XOR")

    val breakingRule2 = gates
      .filterNot(g => g.out.startsWith("z"))
      .filter(_.op == "XOR")
      .filterNot(g => (g.inp1.startsWith("x") || g.inp1.startsWith("y")))

    val switches =
      breakingRule2.map(g => (g.out, findCorrespondingZ(List(g), gates)))

    val switchMap =
      switches.map((k1, k2) => List(k1 -> k2, k2 -> k1)).flatten.toMap

    val fixedGates = gates.map(g => {
      val out = g.out
      if (switchMap.contains(out)) { g.copy(out = switchMap(out)) }
      else g
    })

    val x = parseNumber(wires, 'x')
    val y = parseNumber(wires, 'y')
    val expectedZ = x + y

    val newWires = runGates(fixedGates, wires)
    val actualZ = parseNumber(newWires, 'z')

    val diff = (expectedZ ^ actualZ).toBinaryString
    val indexOfDiff = diff.length() - 1
    val diffInput = "x%02d".format(indexOfDiff)
    val newSwitches =
      gates.filter(g => g.inp1 == diffInput || g.inp2 == diffInput).map(_.out)

    val switchMapNew =
      switchMap ++ Map(
        newSwitches(0) -> newSwitches(1),
        newSwitches(1) -> newSwitches(0)
      )

    val fixedGates2 = gates.map(g => {
      val out = g.out
      if (switchMapNew.contains(out)) { g.copy(out = switchMapNew(out)) }
      else g
    })

    val correctWires = runGates(fixedGates2, wires)
    val actuallyCorrectZ = parseNumber(correctWires, 'z')

    val answer = switchMapNew.keySet.toList.sorted.mkString(",")
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer.toString()
  }
}
