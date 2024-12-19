package adventofscala.y2024

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day18Spec extends AnyFlatSpec with Matchers {

  val year = "2024"
  val day = "18"
  val solution = new Day18

  behavior of "part 1"
  it should "pass the example test" in {
    solution.part1(s"test-inputs/y$year/day$day.txt", 12, 7) should be(22)
  }

  it should "solve for real input" in {
    solution.part1(s"solution-inputs/y$year/day$day.txt", 1024, 71)
  }

  behavior of "part 2"
  it should "pass the example test" in {
    solution.part2(s"test-inputs/y$year/day$day.txt", 12, 7) should be("6,1")
  }

  it should "solve for real input" in {
    solution.part2(s"solution-inputs/y$year/day$day.txt", 1024, 71)
  }
}
