package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day25 extends FileLoader {

  case class Component(name: String, neighbours: Set[String]) {
    def addNeighbours(newNeighbours: Iterable[String]) = {
      Component(name, neighbours ++ newNeighbours)
    }
    def removeNeighbour(neighbour: String): Component = {
      Component(name, neighbours - neighbour)
    }
  }

  @tailrec
  private def parseGraph(
      lines: List[String],
      components: Map[String, Component]
  ): Map[String, Component] = {
    if (lines.isEmpty) components
    else {
      val nextLine = lines.head
      val currentName = nextLine.split(":").head
      val neighboursName = nextLine.split(":")(1).strip().split(" ")
      val currentUpdated = components
        .get(currentName)
        .getOrElse(Component(currentName, Set()))
        .addNeighbours(neighboursName)

      val neighboursUpdated = neighboursName
        .map(name => components.get(name).getOrElse(Component(name, Set())))
        .map(neighbour => neighbour.addNeighbours(List(currentName)))

      val updatedComponents =
        (currentUpdated +: neighboursUpdated).foldLeft(components)((set, c) =>
          set.updated(c.name, c)
        )
      parseGraph(lines.tail, updatedComponents)
    }
  }

  @tailrec
  private def findPath(
      goal: String,
      components: Map[String, Component],
      inprogessPaths: List[List[String]],
      counter: Int
  ): Option[List[String]] = {
    val currentPath = inprogessPaths.head
    if (currentPath.head == goal) Some(currentPath)
    else if (counter > 1000) None
    else {
      val currentComponent = components(currentPath.head)
      val newPaths =
        currentComponent.neighbours
          .filter(n => !currentPath.contains(n))
          .map(_ :: currentPath)
          .toList

      val newInprogress =
        util.Random.shuffle(newPaths) ++ inprogessPaths.tail

      findPath(goal, components, newInprogress, counter + 1)
    }

  }

  def samplePaths(
      n: Int,
      components: Map[String, Component]
  ): List[List[String]] = {
    Range(0, n).foldLeft(List[List[String]]())((l, _) => {
      val startN = util.Random.nextInt(components.size)
      val start = components.keySet.iterator.drop(startN).next()
      val goalN = util.Random.nextInt(components.size)
      val goal = components.keySet.iterator.drop(goalN).next()
      val path = findPath(goal, components, List(List(start)), 1)
      path
        .map(p => {
          p :: l
        })
        .getOrElse(l)
    })
  }

  case class FreqMap(map: Map[(String, String), Int]) {
    def increment(a: String, b: String): FreqMap = {
      val k1 = List(a, b).min
      val k2 = List(a, b).max
      val newVal = map.get((k1, k2)).map(_ + 1).getOrElse(1)
      FreqMap(map.updated((k1, k2), newVal))
    }
  }
  def edgeFrequency(paths: List[List[String]]): Map[(String, String), Int] = {
    paths
      .foldLeft(FreqMap(Map()))((fm, p) =>
        Range(0, p.size - 1).foldLeft(fm)((fmInner, idx) =>
          fmInner.increment(p(idx), p(idx + 1))
        )
      )
      .map
  }

  @tailrec
  private def groupSize(
      visited: Set[String],
      unvisited: Set[String],
      components: Map[String, Component]
  ): Int = {
    if (unvisited.isEmpty) visited.size
    else {
      val current = unvisited.head
      val newUnvisited =
        unvisited.tail ++ components(current).neighbours.filter(n =>
          !visited.contains(n)
        )
      groupSize(visited + current, newUnvisited, components)
    }
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val components = parseGraph(inputList, Map())
    val paths = samplePaths(5000, components)
    val freq = edgeFrequency(paths)
    val mostFrequent =
      freq.toList.sortBy((k, v) => v).takeRight(3).map((k, _) => k)

    val updatedComponents = mostFrequent.foldLeft(components)((c, k) => {
      val c1 = components(k._1).removeNeighbour(k._2)
      val c2 = components(k._2).removeNeighbour(k._1)
      c.updated(k._1, c1).updated(k._2, c2)
    })

    val size =
      groupSize(Set(), Set(updatedComponents.keySet.head), updatedComponents)
    val answer = size * (updatedComponents.size - size)
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

}
