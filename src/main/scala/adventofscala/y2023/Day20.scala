package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day20 extends FileLoader {

  abstract class Pulse(source: String, dest: String) {
    def getSource: String = source
    def getDest: String = dest
  }
  case class HighPulse(source: String, dest: String) extends Pulse(source, dest)
  case class LowPulse(source: String, dest: String) extends Pulse(source, dest)

  abstract class Module(name: String, dest: List[String]) {
    def getName: String = name
    def recievePulse(pulse: Pulse): (Module, List[Pulse])
  }

  case class FlipFlop(name: String, dest: List[String], on: Boolean)
      extends Module(name, dest) {
    def flipped: FlipFlop = FlipFlop(name, dest, !on)
    def recievePulse(pulse: Pulse): (Module, List[Pulse]) = {
      pulse match
        case LowPulse(_, _) => {
          val pulses: List[Pulse] = if (on) { dest.map(LowPulse(name, _)) }
          else { dest.map(HighPulse(name, _)) }
          (flipped, pulses)
        }
        case HighPulse(_, _) => (this, List())
    }
  }

  object FlipFlop {
    def apply(name: String, dest: List[String]): FlipFlop =
      FlipFlop(name, dest, false)
  }

  case class Conjunction(
      name: String,
      dest: List[String],
      memory: Map[String, Boolean]
  ) extends Module(name, dest) {
    def updateMemory(source: String, highPulse: Boolean): Conjunction =
      Conjunction(name, dest, memory.updated(source, highPulse))
    def recievePulse(pulse: Pulse): (Module, List[Pulse]) = {
      val newConjunction: Conjunction = pulse match
        case LowPulse(source, _)  => updateMemory(source, false)
        case HighPulse(source, _) => updateMemory(source, true)

      val newPulses: List[Pulse] =
        if (newConjunction.memory.toSet.forall((_, high) => high))
          dest.map(LowPulse(name, _))
        else dest.map(HighPulse(name, _))

      (newConjunction, newPulses)
    }
    def addInput(input: String): Conjunction =
      Conjunction(name, dest, memory.updated(input, false))
  }

  object Conjunction {
    def apply(name: String, dest: List[String]): Conjunction = {
      Conjunction(name, dest, Map())
    }
  }

  case class Broadcaster(dest: List[String])
      extends Module("broadcaster", dest) {
    def recievePulse(pulse: Pulse): (Module, List[Pulse]) = {
      val newPulses: List[Pulse] = pulse match
        case _: LowPulse  => dest.map(LowPulse(getName, _))
        case _: HighPulse => dest.map(HighPulse(getName, _))

      (this, newPulses)
    }
  }

  def parseNameAndDest(l: String): (String, List[String]) = {
    val m = """(.+) -> (.+)""".r.findFirstMatchIn(l).get
    val destinations: List[String] =
      m.group(2).split(",").toList.map(_.strip())
    (m.group(1), destinations)
  }

  @tailrec
  private def parseModulesRec(
      lines: List[String],
      modules: Map[String, Module]
  ): Map[String, Module] = {
    if (lines.isEmpty) modules
    else {
      val parsed = parseNameAndDest(lines.head)
      val flipPattern = """%(.+)""".r
      val conjPattern = """&(.+)""".r

      val module = parsed._1 match
        case flipPattern(name) => FlipFlop(name, parsed._2)
        case conjPattern(name) => Conjunction(name, parsed._2)
        case "broadcaster"     => Broadcaster(parsed._2)

      val newModules = modules.updated(module.getName, module)
      parseModulesRec(lines.tail, newModules)
    }
  }

  def parseModules(
      lines: List[String]
  ): Map[String, Module] = {
    val modules = parseModulesRec(lines, Map())
    lines.foldLeft(modules)((m, l) => {
      val parsed = parseNameAndDest(l)
      val moduleName =
        if (parsed._1.head == '%' || parsed._1.head == '&') parsed._1.drop(1)
        else parsed._1
      val destinations = parsed._2
      destinations.foldLeft(m)((mInner, d) => {
        val destModule = mInner.get(d)
        if (
          destModule
            .map(module => module.isInstanceOf[Conjunction])
            .getOrElse(false)
        ) {
          mInner.updated(
            d,
            destModule.get.asInstanceOf[Conjunction].addInput(moduleName)
          )
        } else { mInner }
      })
    })
  }

  @tailrec
  private def processPulses(
      pendingPulses: List[Pulse],
      processedPulses: List[Pulse],
      modules: Map[String, Module]
  ): (List[Pulse], Map[String, Module]) = {
    if (pendingPulses.isEmpty) (processedPulses, modules)
    else {
      val currentPulse = pendingPulses.head
      val receivingModule = modules.get(currentPulse.getDest)
      if (receivingModule.isDefined) {
        val pulseResult =
          modules(currentPulse.getDest).recievePulse(currentPulse)
        val newModules = modules.updated(currentPulse.getDest, pulseResult._1)
        val newPendingPulses =
          pendingPulses.tail ::: pulseResult._2
        val newProcessedPulses = currentPulse :: processedPulses
        processPulses(newPendingPulses, newProcessedPulses, newModules)
      } else {
        processPulses(
          pendingPulses.tail,
          currentPulse :: processedPulses,
          modules
        )
      }
    }
  }

  @tailrec
  private def pushTheButton(
      modules: Map[String, Module],
      nLows: Int,
      nHighs: Int,
      pushesRemaining: Int
  ): Int = {
    if (pushesRemaining == 0) nLows * nHighs
    else {
      val pulseRes =
        processPulses(List(LowPulse("button", "broadcaster")), List(), modules)
      val newNLows = nLows + pulseRes._1.count(_.isInstanceOf[LowPulse])
      val newNHighs = nHighs + pulseRes._1.count(_.isInstanceOf[HighPulse])
      pushTheButton(pulseRes._2, newNLows, newNHighs, pushesRemaining - 1)
    }
  }

  @tailrec
  private def findCycles(
      modules: Map[String, Module],
      counter: Long,
      specialModule: String
  ): Long = {
    if (counter % 10000 == 0) { println(s"Pushed button ${counter}") }
    val pulseRes =
      processPulses(List(LowPulse("button", "broadcaster")), List(), modules)

    val specialPulses =
      pulseRes._1.filter(p => p.getSource == specialModule && p.getDest == "ll")
    if (
      specialPulses.exists(p =>
        p
          .isInstanceOf[HighPulse]
      )
    ) {
      println(s"High pulse from $specialModule for counter $counter")
      counter
    } else {
      findCycles(pulseRes._2, counter + 1, specialModule)
    }
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val moduleStart = parseModules(inputList)
    val answer = pushTheButton(moduleStart, 0, 0, 1000)
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(1: BigInt) { (a, b) =>
    b * a / Stream
      .iterate((a, b)) { case (x, y) => (y, x % y) }
      .dropWhile(_._2 != 0)
      .head
      ._1
      .abs
  }

  def part2(inputPath: String): Unit = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val moduleStart = parseModules(inputList)
    val cycles = List("kl", "vm", "kv", "vb")
      .map(findCycles(moduleStart, 1, _))
      .map(BigInt(_))

    val answer = lcm(cycles)

    println(s"${this.getClass()}: The answer to part two is $answer")
  }
}
