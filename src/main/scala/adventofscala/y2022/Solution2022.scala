package adventofscala.y2022

abstract class Solution2022 {
  val year = "2022"
  def day(): String
  val fileName: String = s"y$year/day${day()}.txt"
}
