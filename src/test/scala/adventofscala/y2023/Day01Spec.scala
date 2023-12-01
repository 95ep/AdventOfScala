package adventofscala.y2023

package adventofscala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day01Spec extends AnyFlatSpec with Matchers {

  val day01 = new Day01

  behavior of "part 1"
  it should "pass the example test" in {
    day01.part1("test-inputs/y2023/day01_1.txt") should be(142)
  }

  it should "solve for real input" in {
    day01.part1("solution-inputs/y2023/day01.txt")
  }

  behavior of "part 2"
  it should "pass the example test" in {
    day01.part2("test-inputs/y2023/day01_2.txt") should be(281)
  }

  it should "solve for real input" in {
    day01.part2("solution-inputs/y2023/day01.txt")
  }
}
