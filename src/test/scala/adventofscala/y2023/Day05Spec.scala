package adventofscala.y2023

package adventofscala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day05Spec extends AnyFlatSpec with Matchers {

  val day = new Day05

  behavior of "part 1"
  it should "pass the example test" in {
    day.part1("test-inputs/y2023/day05.txt") should be(35)
  }

  it should "solve for real input" in {
    day.part1("solution-inputs/y2023/day05.txt")
  }

  behavior of "part 2"
  it should "pass the example test" in {
    day.part2("test-inputs/y2023/day05.txt") should be(46)
  }

  it should "solve for real input" in {
    day.part2("solution-inputs/y2023/day05.txt")
  }
}
