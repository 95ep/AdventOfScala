package adventofscala.y2022

@main def main(day: Int): Unit =
  day match
    case 1 => run_day01
    case 2 => run_day02
    case 20 => {
      val day = Day20("inputs")
      day.part1
      day.part2
    }
    case _ => println(s"No solution for day ${day} implemented!")

def run_day01: Unit =
  val d1 = Day01("inputs/y2022/day01.txt")
  d1.solution1
  d1.solution2

def run_day02: Unit =
  val d2 = Day02("inputs/y2022/day02.txt")
  d2.solution1
  d2.solution2
