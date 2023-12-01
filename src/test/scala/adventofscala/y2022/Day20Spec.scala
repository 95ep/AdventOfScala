package adventofscala.y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day20Spec extends AnyFlatSpec with Matchers {

  val dayClass = Day20("test-inputs")

  "part 1" should "solve example input" in {
    dayClass.part1 should be(3)
  }

  "part 2" should "solve example input" in {
    dayClass.part2 should be(1623178306)
  }
}
