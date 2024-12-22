package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.*

class Day22 extends FileLoader {

  def mix(a: Long, b: Long): Long = a ^ b

  def prune(a: Long): Long = a % 16777216

  def nextNumber(a: Long): Long = {
    val step1 = prune(mix(a, a * 64))
    val step2 = prune(mix(step1, step1 / 32))
    prune(mix(step2, step2 * 2048))
  }

  @tailrec
  final def allSecrets(secrets: List[Long], i: Int): List[Long] = {
    if (i == 2001) secrets
    else
      val next = nextNumber(secrets.head)
      allSecrets(next :: secrets, i + 1)
  }

  @tailrec
  final def evalPattern(
      pattern: List[Int],
      prices: List[Int],
      diffs: List[Int]
  ): Long = {
    if (diffs.isEmpty) {
      0L
    } else if (diffs.take(4) == pattern)
      prices(4)
    else evalPattern(pattern, prices.drop(1), diffs.drop(1))
  }

  def part1(inputPath: String): Long = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val initalNumbers = inputList.map(_.toInt)

    val numbers2k = initalNumbers.map(n => allSecrets(List(n), 1).head)
    val answer = numbers2k.sum
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  @tailrec
  final def getPatternsAndScore(
      diffList: List[Int],
      priceList: List[Int],
      patternToScore: Map[List[Int], Int]
  ): Map[List[Int], Int] = {
    if (diffList.size == 3) patternToScore
    else {
      val pattern = diffList.take(4)
      val newP2S =
        if (patternToScore.contains(pattern)) patternToScore
        else patternToScore.updated(pattern, priceList(4))
      getPatternsAndScore(diffList.tail, priceList.tail, newP2S)
    }
  }

  def durationInMs(t0: Long): Long = (System.nanoTime() - t0) / 1000000
  def part2(inputPath: String): Long = {
    val t0 = System.nanoTime()
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val initalNumbers = inputList.map(_.toInt)

    val secrets = initalNumbers.map(n => allSecrets(List(n), 1).reverse)
    println(s"Got all secrets after ${durationInMs(t0)} ms")
    val prices = secrets.map(sl => sl.map(s => (s % 10).toInt))
    val diffs = prices.map(pl => pl.tail.zip(pl).map((a, b) => a - b))
    println(s"Got all diffs after ${durationInMs(t0)} ms")
    val allPatterns2Scores = prices
      .zip(diffs)
      .map((pl, dl) => getPatternsAndScore(dl, pl, Map()))

    println(
      s"Got all patterns and scores per buyer after ${durationInMs(t0)} ms"
    )
    val allPatterns = allPatterns2Scores.map(p2s => p2s.keySet).flatten.toSet
    println(s"Got all unique patterns after ${durationInMs(t0)} ms")
    val scorePerPattern = allPatterns.map(pattern =>
      allPatterns2Scores.map(p2s => p2s.getOrElse(pattern, 0)).sum
    )
    val answer = scorePerPattern.max
    println(s"Got the answer after ${durationInMs(t0)} ms")
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
