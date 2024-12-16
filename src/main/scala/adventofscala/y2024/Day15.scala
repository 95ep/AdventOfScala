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

  // @tailrec
  // final def moveBoxes(
  //     walls: Set[(Int, Int)],
  //     boxes: Set[(Int, Int)],
  //     dir: (Int, Int),
  //     pos: (Int, Int),
  //     part2: Boolean
  // ): Option[(Int, Int)] = {
  //   val altBoxPos = if (pos._2 % 2 == 0) pos else (pos._1, pos._2 - 1)
  //   if (walls(pos) || walls(altBoxPos)) None
  //   else if (!boxes(altBoxPos) && !boxes(altBoxPos)) { Some(pos) }
  //   else {
  //     val newPos = (pos._1 + dir._1, pos._2 + dir._2)
  //     moveBoxes(walls, boxes, dir, newPos, part2)
  //   }
  // }

  @tailrec
  final def moveBoxes(
      toBeMoved: List[(Int, Int)],
      movedBoxes: List[(Int, Int)],
      removedBoxes: List[(Int, Int)],
      boxes: Set[(Int, Int)],
      walls: Set[(Int, Int)],
      dir: (Int, Int),
      part2: Boolean
  ): Option[Set[(Int, Int)]] = {
    if (toBeMoved.isEmpty) Some(boxes -- removedBoxes ++ movedBoxes)
    else {
      val orgPos =
        if (part2)
          List(
            toBeMoved.head,
            (toBeMoved.head._1, toBeMoved.head._2 + 1),
            (toBeMoved.head._1, toBeMoved.head._2 - 1)
          )
        else
          List(toBeMoved.head)

      val newPos: List[(Int, Int)] =
        orgPos.map((i, j) => (i + dir._1, j + dir._2))
      if (newPos.exists(p => walls.contains(p))) None
      else {
        val newBoxesToMove =
          newPos
            .filter(p => boxes.contains(p))
            .filter(p => !toBeMoved.contains(p)) ::: toBeMoved.tail

        moveBoxes(
          newBoxesToMove,
          movedBoxes :+ newPos.head,
          removedBoxes :+ orgPos.filter(p => boxes.contains(p)).head,
          boxes,
          walls,
          dir,
          part2
        )
      }
    }
  }

  @tailrec
  final def runAround(
      instructions: List[Char],
      walls: Set[(Int, Int)],
      boxes: Set[(Int, Int)],
      robot: (Int, Int),
      part2: Boolean
  ): Set[(Int, Int)] = {
    if (instructions.isEmpty) { boxes }
    else {
      val direction = instructions.head match
        case '<' => (0, -1)
        case '>' => (0, 1)
        case 'v' => (1, 0)
        case '^' => (-1, 0)
      val nextPos = (robot._1 + direction._1, robot._2 + direction._2)
      val partFactor = if (part2) 2 else 1
      val nextBoxPos =
        (robot._1 + direction._1, robot._2 + direction._2 * partFactor)
      val movedBoxes =
        if (boxes(nextBoxPos))
          moveBoxes(
            List(nextBoxPos),
            List(),
            List(),
            boxes,
            walls,
            direction,
            part2
          )
        else if (
          (boxes(
            (nextBoxPos._1, nextBoxPos._2 - 1)
          ) && part2)
        )
          moveBoxes(
            List((nextBoxPos._1, nextBoxPos._2 - 1)),
            List(),
            List(),
            boxes,
            walls,
            direction,
            part2
          )
        else None

      // val movedBoxes =
      //   (if (
      //      boxes(nextBoxPos) || (boxes(
      //        (nextBoxPos._1, nextBoxPos._2 - 1)
      //      ) && part2)
      //    )
      //      moveBoxes(
      //        List(nextBoxPos),
      //        List(),
      //        List(),
      //        boxes,
      //        walls,
      //        direction,
      //        part2
      //      )
      //    else None)

      val newPos =
        if (
          (!(walls(nextPos) || (walls(
            (nextPos._1, nextPos._2 - 1)
          ) && part2)) &&
            !(boxes(nextPos) || (boxes(
              (nextPos._1, nextPos._2 - 1)
            ) && part2))) || movedBoxes.isDefined
        ) nextPos
        else robot
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
