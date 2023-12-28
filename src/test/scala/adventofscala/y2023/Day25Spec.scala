package adventofscala.y2023

package adventofscala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day25Spec extends AnyFlatSpec with Matchers {

  val year = "2023"
  val day = "25"
  val solution = new Day25

  behavior of "part 1"
  it should "pass the example test" in {
    solution.part1(s"test-inputs/y$year/day$day.txt") should be(54)
  }

  it should "solve for real input" in {
    solution.part1(s"solution-inputs/y$year/day$day.txt")
  }
}
