package adventofscala.y2024

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day24Spec extends AnyFlatSpec with Matchers {

  val year = "2024"
  val day = "24"
  val solution = new Day24

  behavior of "part 1"
  it should "pass the example test" in {
    solution.part1(s"test-inputs/y$year/day$day.txt") should be(2024)
  }

  it should "solve for real input" in {
    solution.part1(s"solution-inputs/y$year/day$day.txt")
  }

  behavior of "part 2"

  it should "solve for real input" in {
    solution.part2(s"solution-inputs/y$year/day$day.txt")
  }
}
