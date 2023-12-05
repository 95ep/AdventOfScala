package adventofscala.y2023
import adventofscala.utils.FileLoader
import java.util.HashMap

class Day05 extends FileLoader {

  def convertInput(
      input: List[String]
  ): (Array[Long], List[List[Array[Long]]]) = {
    val seeds: Array[Long] =
      input(0).split(": ")(1).split(" ").map(s => s.toLong)

    var idx: Int = 3
    var line: String = ""
    var currentList: List[Array[Long]] = List()
    var allLists: List[List[Array[Long]]] = List()

    while (idx < input.size) {
      line = input(idx)
      if (line != "") {
        if (""".+to.+""".r.matches(line)) {
          allLists = allLists :+ currentList
          currentList = List()
        } else {
          currentList = currentList :+ line.split(" ").map(s => s.toLong)
        }
      }
      idx += 1
    }
    allLists = allLists :+ currentList
    (seeds, allLists)
  }

  def convertSeed(seed: Long, currentMap: List[Array[Long]]): Long = {
    val converted = currentMap
      .map(range =>
        if (range(1) <= seed && seed < (range(1) + range(2))) {
          Some(seed - range(1) + range(0))
        } else None
      )
      .filter(el => el.isDefined)

    if (converted.isEmpty) {
      seed
    } else if (converted.size == 1) {
      converted.head.get
    } else { throw new RuntimeException(s"This is weird. ${converted.size}") }

  }

  def convertSeedReversed(seed: Long, currentMap: List[Array[Long]]): Long = {
    val converted = currentMap
      .map(range =>
        if (range(0) <= seed && seed < (range(0) + range(2))) {
          Some(seed - range(0) + range(1))
        } else None
      )
      .filter(el => el.isDefined)

    if (converted.isEmpty) {
      seed
    } else if (converted.size == 1) {
      converted.head.get
    } else { throw new RuntimeException(s"This is weird. ${converted.size}") }

  }

  def calcLocation(seed: Long, maps: List[List[Array[Long]]]): Long = {
    var variableSeed = seed
    maps.foreach(m => variableSeed = convertSeed(variableSeed, m))
    println(s"Location for seed $seed -> $variableSeed")
    variableSeed
  }

  def seedInRange(seed: Long, allSeeds: Array[Long]): Boolean = {
    allSeeds
      .grouped(2)
      .map(group => group(0) <= seed && seed < group(0) + group(1))
      .exists(b => b)
  }

  def part1(inputPath: String): Long = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val tup = convertInput(inputList)
    val answer = tup._1.map(seed => calcLocation(seed, tup._2)).min

    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Long = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val tup = convertInput(inputList)

    val mapsReversed = tup._2.reverse
    var solved = false
    var location: Long = -1
    while (!solved) {
      location += 1
      var variableSeed: Long = location
      mapsReversed.foreach(m =>
        variableSeed = convertSeedReversed(variableSeed, m)
      )
      // println(s"Original seed for location $location => $variableSeed")
      variableSeed
      solved = seedInRange(variableSeed, tup._1)

    }

    val answer = location

    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
