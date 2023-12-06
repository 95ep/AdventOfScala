package adventofscala.y2023

package adventofscala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day06Spec extends AnyFlatSpec with Matchers {

  val day = new Day06

  behavior of "part 1"
  it should "pass the example test" in {
    day.part1("test-inputs/y2023/day06.txt") should be(288)
  }

  it should "solve for real input" in {
    day.part1("solution-inputs/y2023/day06.txt")
  }

  behavior of "part 2"
  it should "pass the example test" in {
    day.part2("test-inputs/y2023/day06.txt") should be(71503)
  }

  it should "solve for real input" in {
    day.part2("solution-inputs/y2023/day06.txt")
  }
}
