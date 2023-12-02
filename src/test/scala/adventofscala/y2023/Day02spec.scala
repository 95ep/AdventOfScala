package adventofscala.y2023

package adventofscala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day02Spec extends AnyFlatSpec with Matchers {

  val day02 = new Day02

  behavior of "part 1"
  it should "pass the example test" in {
    day02.part1("test-inputs/y2023/day02.txt") should be(8)
  }

  it should "solve for real input" in {
    day02.part1("solution-inputs/y2023/day02.txt")
  }

  behavior of "part 2"
  it should "pass the example test" in {
    day02.part2("test-inputs/y2023/day02.txt") should be(2286)
  }

  it should "solve for real input" in {
    day02.part2("solution-inputs/y2023/day02.txt")
  }
}
