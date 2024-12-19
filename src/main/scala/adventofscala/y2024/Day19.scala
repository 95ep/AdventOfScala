package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.compiletime.ops.string
import scala.annotation.tailrec

def parsePatterns(line: String): List[String] =
  line.split(",").map(_.strip()).toList

@tailrec
final def nDesignSolutions(
    patterns: List[String],
    designs: List[String],
    nSolutions: Long,
    solutionCache: Map[String, Long]
): Long = {
  if (designs.isEmpty) {
    nSolutions
  } else
    val nextDesign = designs.head
    if (nextDesign.isEmpty)
      nDesignSolutions(
        patterns,
        designs.tail,
        nSolutions + 1,
        solutionCache.updated(nextDesign, 1)
      )
    else
      val newDesigns = patterns
        .map(p => nextDesign.stripPrefix(p))
        .filter(d => d != nextDesign)

      if (newDesigns.forall(d => solutionCache.contains(d)))
        val nChildSolutions = newDesigns.map(solutionCache(_)).sum
        nDesignSolutions(
          patterns,
          designs.tail,
          nSolutions + nChildSolutions,
          solutionCache.updated(nextDesign, nChildSolutions)
        )
      else
        nDesignSolutions(
          patterns,
          newDesigns ::: designs.tail,
          nSolutions,
          solutionCache
        )

}

class Day19 extends FileLoader {

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val patterns = parsePatterns(inputList.head)
    val designs = inputList.drop(2)

    val designSolutions =
      designs.map(d => nDesignSolutions(patterns, List(d), 0, Map()))
    val answer = designSolutions.filter(_ > 0).size
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Long = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val patterns = parsePatterns(inputList.head)
    val designs = inputList.drop(2)

    val maxPatter = patterns.map(_.size).max
    println(s"max pattern $maxPatter")

    val designSolutions =
      designs.map(d => nDesignSolutions(patterns, List(d), 0, Map()))

    val answer = designSolutions.sum
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
