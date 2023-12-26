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
  case class Path(tiles: List[Tile]) {
    def contains(tile: Tile): Boolean = tiles.contains(tile)
    def length: Int = tiles.size
    def moveTo(tile: Tile): Path = Path(tile :: tiles)
  }

  def validPos(map: List[List[Char]], t: Tile): Boolean = {
    if (-1 < t.x && t.x < map.size && -1 < t.y && t.y < map.head.size) {
      map(t.x)(t.y) != '#'
    } else
      false
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
      // println(s"${paths.size} active paths")
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

      val newPaths = filteredNeighbours.map(currentPath.moveTo(_)) ++ paths.tail

      val newFinished = filteredNeighbours
        .filter(t => t.x == map.size - 1 && t.y == map.size - 2)
        .map(currentPath.moveTo(_))
        ++ finishedPaths
      findPaths(newPaths, newFinished, map, part1)
    }
  }

  def solution(inputPath: String, part1: Boolean): Int = {
    val inputList: List[List[Char]] =
      loadLines(inputPath).toList.map(_.toCharArray().toList)

    val start = Path(List(Tile(0, 1)))
    val allPaths = findPaths(List(start), List(), inputList, part1)
    allPaths.sortBy(_.length).last.length - 1
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val answer = solution(inputPath, true)
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val answer = solution(inputPath, false)
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
