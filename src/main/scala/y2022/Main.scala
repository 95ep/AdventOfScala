package y2022

@main def main(day: Int): Unit =
  day match
    case 1 => run_day01
    case _ => println(s"No solution for day ${day} implemented!")

def run_day01: Unit =
  val d1 = y2022.day01.Day01("inputs/y2022/day01.txt")
  d1.solution1
  d1.solution2
