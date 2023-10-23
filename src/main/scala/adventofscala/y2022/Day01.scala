package adventofscala.y2022

class Day01(input_path: String):
  var cal_per_elf: List[Int] = List()
  summarize_calories()
  val sorted_cal_per_elf: List[Int] = cal_per_elf.sorted

  private def summarize_calories(): Unit =
    val src = scala.io.Source.fromFile(input_path)
    try
      val lines = src.getLines()
      var current_elf: Int = 0
      var next_food_item: String = ""
      while lines.hasNext do
        next_food_item = lines.next()
        if next_food_item == "" then
          // append current_elf calories to list
          cal_per_elf = current_elf :: cal_per_elf
          current_elf = 0
        else current_elf = current_elf + next_food_item.toInt

      // Append last elf
      cal_per_elf = current_elf :: cal_per_elf
    finally src.close

  def solution1: Unit =
    println(s"Solution to part 1 is: ${sorted_cal_per_elf.last}")

  def solution2: Unit =
    val top_three_elves = sorted_cal_per_elf.takeRight(3)
    var cumulative_calories = 0
    top_three_elves.foreach(x => cumulative_calories += x)
    println(s"Solution to part 2 is: $cumulative_calories")
