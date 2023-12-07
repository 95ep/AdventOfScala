package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.runtime.RichChar

class Day07 extends FileLoader {

  case class Hand(cards: List[Int], bet: Int, partOne: Boolean = true)
      extends Ordered[Hand] {

    def handScore: Int = {
      if (partOne) handScoreCalc(cards)
      else {
        val nJokers = cards.count(c => c == 1)
        val cardsExceptJoker = cards.filter(c => c != 1)
        if (cardsExceptJoker.isEmpty) handScoreCalc(cards)
        else {
          val mostCommonCard =
            cardsExceptJoker.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
          val newCards =
            cardsExceptJoker ::: List.fill(nJokers)(mostCommonCard)
          handScoreCalc(newCards)
        }
      }
    }
    def handScoreCalc(cards: List[Int]): Int = {
      val nUnique = cards.distinct.size
      nUnique match
        case 1 => 7 // five of a kind
        case 2 => { // four of a kind or full house
          if (cards.groupBy(identity).mapValues(_.size).toMap.values.max == 4)
            6
          else 5
        }
        case 3 => { // three or two pair
          if (cards.groupBy(identity).mapValues(_.size).toMap.values.max == 3) 4
          else 3
        }
        case 4 => 2 // one pair
        case 5 => 1 // high card
    }

    def handScorePart1: Int = {
      val nUnique = cards.distinct.size
      nUnique match
        case 1 => 7 // five of a kind
        case 2 => { // four of a kind or full house
          if (cards.groupBy(identity).mapValues(_.size).toMap.values.max == 4)
            6
          else 5
        }
        case 3 => { // three or two pair
          if (cards.groupBy(identity).mapValues(_.size).toMap.values.max == 3) 4
          else 3
        }
        case 4 => 2 // one pair
        case 5 => 1 // high card
    }

    def compare(that: Hand): Int = {
      if (this.handScore != that.handScore) {
        this.handScore.compare(that.handScore)
      } else {
        val cardComps: List[Int] =
          this.cards
            .zip(that.cards)
            .map((thisCard: Int, thatCard: Int) => thisCard.compare(thatCard))
            .filter(_ != 0)
        if (cardComps.nonEmpty) cardComps.head
        else 0
      }
    }
  }

  def parseHand(line: String, partOne: Boolean = true): Hand = {
    val cardsString = line.split(" ").head
    val bet = line.split(" ")(1).toInt

    val cards: List[Int] = cardsString
      .map(c =>
        if (c.isDigit) c.asDigit
        else {
          c match
            case 'T' => 10
            case 'J' => if (partOne) 11 else 1
            case 'Q' => 12
            case 'K' => 13
            case 'A' => 14
        }
      )
      .toList

    Hand(cards, bet, partOne)
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList

    val sortedHands = inputList
      .map(l => parseHand(l, true))
      .sorted
    val answer = sortedHands.zipWithIndex
      .map((hand, idx) => hand.bet * (idx + 1))
      .sum
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList

    val sortedHands = inputList
      .map(l => parseHand(l, false))
      .sorted
    val answer = sortedHands.zipWithIndex
      .map((hand, idx) => hand.bet * (idx + 1))
      .sum

    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
