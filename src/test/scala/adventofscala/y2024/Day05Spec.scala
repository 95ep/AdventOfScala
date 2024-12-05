package adventofscala.y2024

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day05Spec extends AnyFlatSpec with Matchers {

  val year = "2024"
  val day = "05"
  val solution = new Day05

  behavior of "part 1"
  it should "pass the example test" in {
    solution.part1(s"test-inputs/y$year/day$day.txt") should be(143)
  }

  it should "solve for real input" in {
    solution.part1(s"solution-inputs/y$year/day$day.txt")
  }

  behavior of "part 2"
  it should "pass the example test" in {
    solution.part2(s"test-inputs/y$year/day$day.txt") should be(123)
  }

  it should "solve for real input" in {
    solution.part2(s"solution-inputs/y$year/day$day.txt")
  }
}
