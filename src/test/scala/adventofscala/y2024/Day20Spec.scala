package adventofscala.y2024

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day20Spec extends AnyFlatSpec with Matchers {

  val year = "2024"
  val day = "20"
  val solution = new Day20

  behavior of "part 1"
  it should "pass the example test" in {
    solution.part1(s"test-inputs/y$year/day$day.txt", (3, 1), (7, 5)) should be(
      0
    )
  }

  it should "solve for real input" in {
    solution.part1(s"solution-inputs/y$year/day$day.txt", (25, 59), (47, 53))
  }

  behavior of "part 2"
  it should "pass the example test" in {
    solution.part2(s"test-inputs/y$year/day$day.txt", (3, 1), (7, 5)) should be(
      0
    )
  }

  it should "solve for real input" in {
    // 524981 too low
    solution.part2(s"solution-inputs/y$year/day$day.txt", (25, 59), (47, 53))
  }
}
