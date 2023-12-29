package adventofscala.y2023

package adventofscala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day21Spec extends AnyFlatSpec with Matchers {

  val year = "2023"
  val day = "21"
  val solution = new Day21

  behavior of "part 1"
  it should "pass the example test" in {
    solution.part1(s"test-inputs/y$year/day$day.txt", 6) should be(16)
  }

  it should "solve for real input" in {
    solution.part1(s"solution-inputs/y$year/day$day.txt", 64)
  }

  behavior of "part 2"
  it should "solve for real input" in { // 289343201230140 too low
    solution.part2(s"solution-inputs/y$year/day$day.txt")
  }
}
