package adventofscala.y2024
import adventofscala.utils.FileLoader

class Day25 extends FileLoader {

  def parseKey(keyBlock: List[String]): Seq[Int] = {
    val columns =
      for i <- 0 until keyBlock.head.size
      yield for j <- 0 until keyBlock.size yield keyBlock(j)(i)

    columns.map(col => col.count(_ == '#') - 1)
  }

  def parseLocksAndKey(inp: List[String]): (Seq[Seq[Int]], Seq[Seq[Int]]) = {
    val keys = for
      i <- 0 until inp.size by 8
      keyBlock = inp.drop(i).take(7) if keyBlock.last == "#####"
    yield parseKey(keyBlock)
    val locks = for
      i <- 0 until inp.size by 8
      keyBlock = inp.drop(i).take(7) if keyBlock.head == "#####"
    yield parseKey(keyBlock)

    (locks, keys)
  }

  def fits(lock: Seq[Int], key: Seq[Int]): Boolean = {
    lock.zip(key).map((l, k) => l + k).forall(_ < 6)
  }
  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val (locks, keys) = parseLocksAndKey(inputList)

    val t = for
      lock <- locks
      key <- keys if fits(lock, key)
    yield 1

    val answer = t.sum
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }
}
