package y2022

package day02

class Day02(input_path: String):
  var instructions: List[Tuple] = List()

  parse_input()

  private def parse_input(): Unit =
    val src = scala.io.Source.fromFile(input_path)
    val instruction_patter = "([A-C]) ([X-Z])".r
    try
      val lines = src.getLines()
      while lines.hasNext do
        val line: String = lines.next()
        val result = instruction_patter.findFirstMatchIn(line)
        if result.isDefined then
          val instruction_match = result.get
          instructions = instructions :+ (
            instruction_match
              .group(1),
            instruction_match.group(2)
          )

    finally src.close

  private def convert_to_num(their_move: String, our_move: String): (Int, Int) =
    val their_num_move = their_move(0).toInt - 'A'.toInt
    val our_num_move = our_move(0).toInt - 'X'.toInt
    val instructions = 0
    (their_num_move, our_num_move)

  private def determine_outcome(their_move: Int, our_move: Int): Int =
    if their_move == our_move then 3
    else if (their_move + 1) % 3 == our_move then 6
    else 0

  private def calc_score_for_round(their_move: Int, our_move: Int): Int =
    var round_score: Int = our_move + 1
    round_score += determine_outcome(their_move, our_move)
    round_score

  private def determine_move(their_move: Int, desired_outcome: Int): Int =
    if desired_outcome == 0 then
      ((their_move + 3) - 1) % 3 // add 3 to avoid negative move vals
    else if desired_outcome == 1 then their_move
    else (their_move + 1) % 3

  def solution1: Unit =
    var score: Int = 0
    instructions.foreach { tup =>
      val (x: String, y: String) = tup: @unchecked
      val (their_move, our_move) = convert_to_num(x, y)
      score += calc_score_for_round(their_move, our_move)
    }
    println(s"Solution to part 1 is: ${score}")

  def solution2: Unit =
    var score: Int = 0
    instructions.foreach { tup =>
      val (x: String, y: String) = tup: @unchecked
      val (their_move, outcome) = convert_to_num(x, y)
      val our_move: Int = determine_move(their_move, outcome)
      score += calc_score_for_round(their_move, our_move)
    }
    println(s"Solution to part 2 is: ${score}")
