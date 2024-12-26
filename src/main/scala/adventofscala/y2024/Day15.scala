package adventofscala.y2024
import adventofscala.utils.FileLoader
import scala.annotation.tailrec

class Day15 extends FileLoader {

  @tailrec
  final def parseMap(
      i: Int,
      j: Int,
      inputList: List[String],
      walls: Set[(Int, Int)],
      boxes: Set[(Int, Int)],
      robot: (Int, Int),
      part2: Boolean
  ): (Set[(Int, Int)], Set[(Int, Int)], (Int, Int)) = {
    if (inputList(i) == "") (walls, boxes, robot)
    else {
      val incrementer = if (part2) 2 else 1
      val i2 = if (j + 1 == inputList(i).size) i + 1 else i
      val j2 = if (j + 1 == inputList(i).size) 0 else j + 1
      val c = inputList(i)(j)
      val additions =
        if (part2) Set((i, incrementer * j), (i, incrementer * j + 1))
        else Set((i, j))
      if (c == '#') {
        parseMap(i2, j2, inputList, walls ++ additions, boxes, robot, part2)
      } else if (c == 'O') {
        parseMap(
          i2,
          j2,
          inputList,
          walls,
          boxes incl (i, incrementer * j),
          robot,
          part2
        )
      } else if (c == '@') {
        parseMap(i2, j2, inputList, walls, boxes, (i, incrementer * j), part2)
      } else
        parseMap(i2, j2, inputList, walls, boxes, robot, part2)
    }
  }

  def collideWithWall(
      pos: (Int, Int),
      walls: Set[(Int, Int)],
      part2: Boolean
  ): Boolean = {
    val coordsOccupied = if (part2) Set(pos, (pos._1, pos._2 + 1)) else Set(pos)
    val wallsOccupies =
      if (part2) walls.map(w => List(w)).flatten else walls

    (coordsOccupied & wallsOccupies).nonEmpty
  }

  def collidedBoxes( // Box to box collisions
      pos: (Int, Int),
      boxes: Set[(Int, Int)],
      part2: Boolean
  ): Set[(Int, Int)] = {
    if (part2)
      val coordsOccupied = Set(pos, (pos._1, pos._2 + 1))
      boxes.filter((b1, b2) => {
        val occupiedByBox = Set((b1, b2), (b1, b2 + 1))
        (occupiedByBox & coordsOccupied).nonEmpty
      })
    else boxes.filter(b => b == pos)
  }

  def robotCollidedBoxes( // Robot to box collisions
      pos: (Int, Int),
      boxes: Set[(Int, Int)],
      part2: Boolean
  ): Set[(Int, Int)] = {
    if (part2)
      val coordsOccupied = Set(pos)
      boxes.filter((b1, b2) => {
        val occupiedByBox = Set((b1, b2), (b1, b2 + 1))
        (occupiedByBox & coordsOccupied).nonEmpty
      })
    else boxes.filter(b => b == pos)
  }

  @tailrec
  final def moveBoxes(
      toBeMoved: List[(Int, Int)],
      movedBoxes: Set[(Int, Int)],
      removedBoxes: Set[(Int, Int)],
      boxes: Set[(Int, Int)],
      walls: Set[(Int, Int)],
      dir: (Int, Int),
      part2: Boolean
  ): Option[Set[(Int, Int)]] = {
    if (toBeMoved.isEmpty) Some(boxes -- removedBoxes ++ movedBoxes)
    else {
      val currentPos = toBeMoved.head
      val nextPos = (currentPos._1 + dir._1, currentPos._2 + dir._2)

      if (collideWithWall(nextPos, walls, part2)) None
      else {
        val newBoxesToMove =
          collidedBoxes(nextPos, boxes - currentPos, part2)

        moveBoxes(
          toBeMoved.tail ++ newBoxesToMove,
          movedBoxes + nextPos,
          removedBoxes + currentPos,
          boxes,
          walls,
          dir,
          part2
        )
      }
    }
  }

  def addWalls(
      walls: Set[(Int, Int)],
      mapMatrix: Seq[Seq[String]]
  ): Seq[Seq[String]] = {
    if (walls.isEmpty) mapMatrix
    else {
      val wall = walls.head
      val newMap =
        mapMatrix.updated(wall._1, mapMatrix(wall._1).updated(wall._2, "#"))
      addWalls(walls.tail, newMap)
    }
  }

  def addBoxes(
      boxes: Set[(Int, Int)],
      mapMatrix: Seq[Seq[String]]
  ): Seq[Seq[String]] = {
    if (boxes.isEmpty) mapMatrix
    else {
      val wall = boxes.head
      val newMapTmp =
        mapMatrix.updated(wall._1, mapMatrix(wall._1).updated(wall._2, "["))
      val newMap =
        newMapTmp.updated(wall._1, newMapTmp(wall._1).updated(wall._2 + 1, "]"))
      addBoxes(boxes.tail, newMap)
    }
  }

  def print(
      walls: Set[(Int, Int)],
      boxes: Set[(Int, Int)],
      robot: (Int, Int)
  ) = {
    val maxX = walls.map(_._1).max
    val maxY = walls.map(_._2).max
    val mapMatrix =
      for i <- 0 until maxX + 1
      yield (for j <- 0 until (maxY + 1) yield ".")

    val withWalls = addWalls(walls, mapMatrix)
    val withBoxes = addBoxes(boxes, withWalls)
    val withRobot =
      withBoxes.updated(robot._1, withBoxes(robot._1).updated(robot._2, "@"))

    withRobot.foreach(l => println(l.mkString))
    println()

  }

  @tailrec
  final def runAround(
      instructions: List[Char],
      walls: Set[(Int, Int)],
      boxes: Set[(Int, Int)],
      robot: (Int, Int),
      part2: Boolean
  ): Set[(Int, Int)] = {
    // print(walls, boxes, robot)
    if (instructions.isEmpty) { boxes }
    else {
      val direction = instructions.head match
        case '<' => (0, -1)
        case '>' => (0, 1)
        case 'v' => (1, 0)
        case '^' => (-1, 0)
      val nextPos = (robot._1 + direction._1, robot._2 + direction._2)
      val boxToMove = robotCollidedBoxes(nextPos, boxes, part2)
      val movedBoxes =
        moveBoxes(
          boxToMove.toList,
          Set(),
          Set(),
          boxes,
          walls,
          direction,
          part2
        )

      val newPos =
        if (walls.contains(nextPos) || movedBoxes.isEmpty) robot
        else nextPos

      // println(instructions.head)
      runAround(
        instructions.tail,
        walls,
        movedBoxes.getOrElse(boxes),
        newPos,
        part2
      )

    }

  }

  def calcGPSScore(boxes: Set[(Int, Int)]): Int = {
    boxes.map((i, j) => 100 * i + j).sum
  }
  def calcGPSScore2(boxes: Set[(Int, Int)], walls: Set[(Int, Int)]): Int = {
    val maxX = walls.map((i, _) => i).max
    val maxY = walls.map((_, j) => j).max

    boxes.toList
      .map((i, j) => {
        val xDist = List(i, maxX - i).min
        val yDist = List(j - 1, maxY - j - 2).min
        yDist * 100 + xDist
      })
      .sum
  }

  def part1(inputPath: String): Int = {
    println("Running part 1")
    val inputList: List[String] = loadLines(inputPath).toList
    val (walls, boxes, robot) =
      parseMap(0, 0, inputList, Set(), Set(), (0, 0), false)
    val instructions = inputList
      .filter(s =>
        s.contains('<') || s.contains('>') || s.contains('v') || s.contains('^')
      )
      .flatten

    val afterMove = runAround(instructions, walls, boxes, robot, false)
    val answer = calcGPSScore(afterMove)
    println(s"${this.getClass()}: The answer to part one is $answer")
    answer
  }

  def part2(inputPath: String): Int = {
    println("Running part 2")
    val inputList: List[String] = loadLines(inputPath).toList
    val (walls, boxes, robot) =
      parseMap(0, 0, inputList, Set(), Set(), (0, 0), true)

    val instructions = inputList
      .filter(s =>
        s.contains('<') || s.contains('>') || s.contains('v') || s.contains('^')
      )
      .flatten

    val afterMove = runAround(instructions, walls, boxes, robot, true)
    afterMove((1, 2))
    val answer = calcGPSScore(afterMove)
    println(s"${this.getClass()}: The answer to part two is $answer")
    answer
  }
}
