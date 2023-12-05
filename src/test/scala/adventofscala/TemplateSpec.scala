package adventofscala.y20xx

package adventofscala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DayXXSpec extends AnyFlatSpec with Matchers {

  val day = new DayXX

  behavior of "part 1"
  it should "pass the example test" in {
    day.part1("test-inputs/y20xx/dayXX.txt") should be(1)
  }

  it should "solve for real input" in {
    day.part1("solution-inputs/y20xx/dayXX.txt")
  }

  behavior of "part 2"
  it should "pass the example test" in {
    day.part2("test-inputs/y20xx/dayXX.txt") should be(1)
  }

  it should "solve for real input" in {
    day.part2("solution-inputs/y20xx/dayXX.txt")
  }
}
