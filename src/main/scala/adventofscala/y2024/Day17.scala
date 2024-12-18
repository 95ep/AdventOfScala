package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec
import scala.concurrent.{Future, ExecutionContext, Await}
import ExecutionContext.Implicits.global
import scala.concurrent.duration.*
class Day17 extends FileLoader {

  case class ChronospatialComputer(
      a: Long,
      b: Long,
      c: Long,
      instructions: List[Int],
      pointer: Int,
      output: String
  ) {
    def adv(op: Int): ChronospatialComputer = {
      val newA = (a / scala.math.pow(2, comboOperand(op))).toInt
      ChronospatialComputer(newA, b, c, instructions, pointer + 2, output)
    }

    def bxl(op: Int): ChronospatialComputer = {
      val newB = b ^ op
      ChronospatialComputer(a, newB, c, instructions, pointer + 2, output)
    }
    def bst(op: Int): ChronospatialComputer = {
      val newB = comboOperand(op) % 8
      ChronospatialComputer(a, newB, c, instructions, pointer + 2, output)
    }

    def jnz(op: Int): ChronospatialComputer = {
      if (a == 0)
        ChronospatialComputer(a, b, c, instructions, pointer + 2, output)
      else ChronospatialComputer(a, b, c, instructions, op, output)
    }

    def bxc(op: Int): ChronospatialComputer = {
      val newB = b ^ c
      ChronospatialComputer(a, newB, c, instructions, pointer + 2, output)
    }

    def out(op: Int): ChronospatialComputer = {
      val newVal = comboOperand(op) % 8
      val newOut =
        if (output.isEmpty()) newVal.toString()
        else output + "," + newVal.toString()
      ChronospatialComputer(a, b, c, instructions, pointer + 2, newOut)
    }

    def bdv(op: Int): ChronospatialComputer = {
      val newB = (a / scala.math.pow(2, comboOperand(op))).toInt
      ChronospatialComputer(a, newB, c, instructions, pointer + 2, output)
    }

    def cdv(op: Int): ChronospatialComputer = {
      val newC = (a / scala.math.pow(2, comboOperand(op))).toInt
      ChronospatialComputer(a, b, newC, instructions, pointer + 2, output)
    }

    def comboOperand(op: Int): Long = {
      op match
        case 0 => 0
        case 1 => 1
        case 2 => 2
        case 3 => 3
        case 4 => a
        case 5 => b
        case 6 => c
    }

    def runInstruction: ChronospatialComputer = {
      val opCode = instructions(pointer)
      val operand = instructions(pointer + 1)
      opCode match
        case 0 => adv(operand)
        case 1 => bxl(operand)
        case 2 => bst(operand)
        case 3 => jnz(operand)
        case 4 => bxc(operand)
        case 5 => out(operand)
        case 6 => bdv(operand)
        case 7 => cdv(operand)
    }

  }

  @tailrec
  final def runInstructions(c: ChronospatialComputer): ChronospatialComputer = {
    if (c.pointer == c.instructions.size)
      c
    else
      runInstructions(c.runInstruction)
  }

  def parse(inputList: List[String]): ChronospatialComputer = {
    val a = inputList(0).split(":")(1).strip().toInt
    val b = inputList(1).split(":")(1).strip().toInt
    val c = inputList(2).split(":")(1).strip().toInt

    val instructions =
      inputList(4).split(":")(1).strip().split(",").map(_.toInt).toList

    ChronospatialComputer(a, b, c, instructions, 0, "")
  }

  def part1(inputPath: String): String = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val c = parse(inputList)
    val completedC = runInstructions(c)

    val answer = completedC.output
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val c = parse(inputList)
    val start = 15000000000001L
    val outputs = Future
      .sequence(
        Range(0, 500)
          .map(i => {
            Future {
              val cVar = c.copy(a = start + i)
              (i, runInstructions(cVar).output)
            }
          })
      )
    outputs.foreach(o => o.foreach(println))
    val matchingOutputs =
      Await
        .result(outputs, 60.seconds)
        .filter((i, o) => o == c.instructions.mkString(","))
    val answer = matchingOutputs.head._1
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
