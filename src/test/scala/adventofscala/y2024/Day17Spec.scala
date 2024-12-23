package adventofscala.y2024

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day17Spec extends AnyFlatSpec with Matchers {

  val year = "2024"
  val day = "17"
  val solution = new Day17

  behavior of "part 1"
  it should "do stuff" in {
    val c = new solution.ChronospatialComputer(
      0,
      29,
      0,
      List(1, 7),
      0,
      ""
    )
    val newC = solution.runInstructions(c)
    // assert(newC.output == "4,2,5,6,7,7,7,7,3,1,0")
    assert(newC.b == 26)
  }

  it should "pass the example test" in {
    solution.part1(s"test-inputs/y$year/day$day.txt") should be(
      "4,6,3,5,6,3,5,2,1,0"
    )
  }

  it should "solve for real input" in {
    // Not 7,3,0,3,5,7,1,5,4
    solution.part1(s"solution-inputs/y$year/day$day.txt")
  }

  behavior of "part 2"

  it should "solve for real input" in {
    solution.part2(s"solution-inputs/y$year/day$day.txt")
  }
}
