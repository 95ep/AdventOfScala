package adventofscala.y2023

package adventofscala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day18Spec extends AnyFlatSpec with Matchers {

  val year = "2023"
  val day = "18"
  val solution = new Day18

  behavior of "part 1"
  it should "pass the example test" in {
    solution.part1(s"test-inputs/y$year/day$day.txt") should be(62L)
  }

  it should "solve for real input" in {
    solution.part1(s"solution-inputs/y$year/day$day.txt")
  }

  behavior of "part 2"
  it should "pass the example test" in {
    solution.part2(s"test-inputs/y$year/day$day.txt") should be(952408144115L)
  }

  it should "solve for real input" in {
    solution.part2(s"solution-inputs/y$year/day$day.txt")
  }
}
