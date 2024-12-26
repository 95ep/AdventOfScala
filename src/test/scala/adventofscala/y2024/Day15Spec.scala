package adventofscala.y2024

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day15Spec extends AnyFlatSpec with Matchers {

  val year = "2024"
  val day = "15"
  val solution = new Day15

  behavior of "part 1"
  it should "pass the example test" in {
    solution.part1(s"test-inputs/y$year/day$day.txt") should be(2028)
  }

  it should "pass the second example test" in {
    solution.part1(s"test-inputs/y$year/day${day}_2.txt") should be(10092)
  }

  it should "solve for real input" in {
    solution.part1(s"solution-inputs/y$year/day$day.txt")
  }

  behavior of "part 2"
  it should "pass the example test" in {
    solution.part2(s"test-inputs/y$year/day${day}_2.txt") should be(9021)
  }
  it should "pass the small example test" in {
    solution.part2(s"test-inputs/y$year/day${day}_3.txt") should be(618)
  }

  it should "solve for real input" in {
    solution.part2(s"solution-inputs/y$year/day$day.txt")
  }
}
