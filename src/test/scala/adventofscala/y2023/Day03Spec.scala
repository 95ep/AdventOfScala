package adventofscala.y2023

package adventofscala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day03Spec extends AnyFlatSpec with Matchers {

  val day03 = new Day03

  behavior of "part 1"
  it should "pass the example test" in {
    day03.part1("test-inputs/y2023/day03.txt") should be(4361)
  }

  it should "solve for real input" in {
    day03.part1("solution-inputs/y2023/day03.txt")
  }

  behavior of "part 2"
  it should "pass the example test" in {
    day03.part2("test-inputs/y2023/day03.txt") should be(467835)
  }

  it should "solve for real input" in {
    day03.part2("solution-inputs/y2023/day03.txt")
  }
}
