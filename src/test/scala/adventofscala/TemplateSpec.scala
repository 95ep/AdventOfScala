package adventofscala.y20xx

package adventofscala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DayXXSpec extends AnyFlatSpec with Matchers {

  val year = "20xx"
  val day = "XX"
  val solution = new DayXX

  behavior of "part 1"
  it should "pass the example test" in {
    solution.part1(s"test-inputs/y$year/day$day.txt") should be(1)
  }

  it should "solve for real input" in {
    solution.part1("solution-inputs/y$year/day$day.txt")
  }

  behavior of "part 2"
  it should "pass the example test" in {
    solution.part2("test-inputs/y$year/day$day.txt") should be(1)
  }

  it should "solve for real input" in {
    solution.part2("solution-inputs/y$year/day$day.txt")
  }
}
