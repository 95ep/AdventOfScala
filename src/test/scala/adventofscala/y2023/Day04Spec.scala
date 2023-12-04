package adventofscala.y2023

package adventofscala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day04Spec extends AnyFlatSpec with Matchers {

  val day04 = new Day04

  behavior of "part 1"
  it should "pass the example test" in {
    day04.part1("test-inputs/y2023/day04.txt") should be(13)
  }

  it should "solve for real input" in {
    day04.part1("solution-inputs/y2023/day04.txt")
  }

  behavior of "part 2"
  it should "pass the example test" in {
    day04.part2("test-inputs/y2023/day04.txt") should be(30)
  }

  it should "solve for real input" in {
    day04.part2("solution-inputs/y2023/day04.txt")
  }
}
