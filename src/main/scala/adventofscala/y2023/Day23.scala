package adventofscala.y2023
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day23 extends FileLoader {

  case class Tile(x: Int, y: Int) {
    def up: Tile = Tile(x - 1, y)
    def down: Tile = Tile(x + 1, y)
    def left: Tile = Tile(x, y - 1)
    def right: Tile = Tile(x, y + 1)
  }
  case class Path(tiles: List[Tile], length: Int) {
    def contains(tile: Tile): Boolean = tiles.contains(tile)
    def moveTo(tile: Tile, dist: Int): Path = Path(tile :: tiles, length + dist)
  }

  def validPos(map: List[List[Char]], t: Tile): Boolean = {
    if (-1 < t.x && t.x < map.size && -1 < t.y && t.y < map.head.size) {
      map(t.x)(t.y) != '#'
    } else
      false
  }

  def getNeighbours(currentTile: Tile, map: List[List[Char]]) = {
    List(
      currentTile.left,
      currentTile.right,
      currentTile.up,
      currentTile.down
    )
      .filter(t => validPos(map, t))
  }

  @tailrec
  private def nextFork(
      current: Tile,
      prev: Tile,
      dist: Int,
      map: List[List[Char]]
  ): (Tile, Int) = {
    val neighbours = getNeighbours(current, map).filter(t => t != prev)
    if (neighbours.size > 1 || neighbours.isEmpty) (current, dist)
    else nextFork(neighbours.head, current, dist + 1, map)
  }

  @tailrec
  private def findForks(
      forks: Map[Tile, List[(Tile, Int)]],
      visited: Set[Tile],
      unvisited: Set[Tile],
      map: List[List[Char]]
  ): Map[Tile, List[(Tile, Int)]] = {
    if (unvisited.isEmpty) forks
    else {
      val current: Tile = unvisited.head
      val forkRes =
        getNeighbours(current, map)
          .map(n => nextFork(n, current, 1, map))
          .filter(f => !visited.contains(f._1))
      val newForks = forkRes.foldLeft(forks)((f, n) =>
        f.updated(current, f.get(current).map(n :: _).getOrElse(List(n)))
          .updated(
            n._1,
            f.get(n._1)
              .map((current, n._2) :: _)
              .getOrElse(List((current, n._2)))
          )
      )
      val newUnvisited =
        unvisited.tail ++ forkRes.map(_._1).filter(t => !visited.contains(t))
      findForks(newForks, visited + current, newUnvisited, map)
    }
  }

  @tailrec
  private def findPaths(
      paths: List[Path],
      finishedPaths: List[Path],
      map: List[List[Char]],
      part1: Boolean
  ): List[Path] = {
    if (paths.isEmpty) finishedPaths
    else {
      val currentPath = paths.head
      val currentTile = currentPath.tiles.head
      val neighbours = if (part1) {
        map(currentTile.x)(currentTile.y) match
          case '^' => List(currentTile.up)
          case '>' => List(currentTile.right)
          case 'v' => List(currentTile.down)
          case '<' => List(currentTile.left)
          case _ =>
            List(
              currentTile.left,
              currentTile.right,
              currentTile.up,
              currentTile.down
            )
      } else
        List(
          currentTile.left,
          currentTile.right,
          currentTile.up,
          currentTile.down
        )

      val filteredNeighbours = neighbours
        .filter(n => !currentPath.contains(n))
        .filter(n => validPos(map, n))

      val newPaths =
        filteredNeighbours.map(currentPath.moveTo(_, 1)) ++ paths.tail

      val newFinished = filteredNeighbours
        .filter(t => t.x == map.size - 1 && t.y == map.size - 2)
        .map(currentPath.moveTo(_, 1))
        ++ finishedPaths
      findPaths(newPaths, newFinished, map, part1)
    }
  }

  @tailrec
  private def findPaths2(
      paths: List[Path],
      finishedPaths: List[Path],
      forks: Map[Tile, List[(Tile, Int)]],
      map: List[List[Char]]
  ): List[Path] = {
    if (paths.isEmpty) finishedPaths
    else {
      val currentPath = paths.head
      val currentTile = currentPath.tiles.head
      val neighbours =
        forks(currentTile).filter((t, _) => !currentPath.tiles.contains(t))

      val newFinished =
        neighbours
          .filter((t, _) => t.x == map.size - 1 && t.y == map.size - 2)
          .map((t, d) => currentPath.moveTo(t, d)) ++ finishedPaths
      val newPaths =
        neighbours.map(n => currentPath.moveTo(n._1, n._2)) ++ paths.tail

      findPaths2(newPaths, newFinished, forks, map)
    }
  }

  def solution(inputPath: String, part1: Boolean): Int = {
    val inputList: List[List[Char]] =
      loadLines(inputPath).toList.map(_.toCharArray().toList)

    val start = Path(List(Tile(0, 1)), 0)
    val allPaths = findPaths(List(start), List(), inputList, part1)
    allPaths.sortBy(_.length).last.length
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val answer = solution(inputPath, true)
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[List[Char]] =
      loadLines(inputPath).toList.map(_.toCharArray().toList)

    val start = Tile(0, 1)
    val forks = findForks(Map(), Set(), Set(start), inputList)
    val allPaths =
      findPaths2(List(Path(List(start), 0)), List(), forks, inputList)
    val answer = allPaths.map(_.length).sorted.last
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
