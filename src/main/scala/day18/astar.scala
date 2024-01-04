package day18

import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

trait HasAdjacencies[T] {
  def adjacencies(): Seq[T]
}

object HelpersTest {
  def main(args: Array[String]) = {

    val start = System.currentTimeMillis()
    for (_ <- (0 to 10)) {
      val iStart = System.currentTimeMillis()
      fivebyfiveTests()
      biggerTest()
      openTest()
      println(System.currentTimeMillis() - iStart)
    }
    val end = System.currentTimeMillis()

    println("total: " + (end - start))
  }

  def parseGrid(s: List[String]): List[TwoDPoint] = {
    val numRows = s.length
    val numCols = s(0).length()

    s.zipWithIndex
      .map(line => {
        line._1.zipWithIndex.map(ch => {
          ch._1 match {
            case '.' => None
            case _   => Some(TwoDPoint.rc(line._2, ch._2))
          }
        })
      })
      .flatten
      .filter(o => o.isDefined)
      .map(o => o.get)
      .toList
  }

  def openTest() = {
    val rawGrid = List(
      //  0123456789A123456789
      /* * */ "+--------------------+",
      /* 0 */ "|....................|",
      /* 1 */ "|....................|",
      /* 2 */ "|...........##.......|",
      /* 3 */ "|.............##.....|",
      /* 4 */ "|.........#.#.#......|",
      /* 5 */ "|.....#....#.##......|",
      /* 6 */ "|...######..#..#.....|",
      /* 7 */ "|...#.....##.#.......|",
      /* 8 */ "|....................|",
      /* 9 */ "|....................|",
      /* * */ "+--------------------+"
    )

    val maze = parseGrid(rawGrid)
      .map(p => point(p.r - 1, p.c - 1)) // shift grid up/left so that 0,0 is in-bounds

    val result = runCourse(point(0, 0), point(8, 15), maze)
    assert(result.isDefined)
    assert(result.get.costToReach == 23)

    val result2 = runCourse(point(5, 8), point(6, 12), maze)
    assert(result2.isDefined)
    assert(result2.get.costToReach == 21)

    val result3 = runCourse(point(5, 8), point(5, 11), maze)
    assert(result3.isEmpty)

  }

  def biggerTest() = {
    val rawGrid = List(
      //  0123456789A123456789
      /* * */ "+--------------------+",
      /* 0 */ "|.###...#.#.#....#...|",
      /* 1 */ "|..##.##..#...##...##|",
      /* 2 */ "|.#......##..#.....#.|",
      /* 3 */ "|...##..#.#.#.###..#.|",
      /* 4 */ "|####.....#..........|",
      /* 5 */ "|...#####.#.#########|",
      /* 6 */ "|.#.........#........|",
      /* 7 */ "|.#######.#.#.#.##.#.|",
      /* 8 */ "|.#...#.#.#.#.##.###.|",
      /* 9 */ "|...#.#.#.#...#......|",
      /* * */ "+--------------------+"
    )

    val maze = parseGrid(rawGrid)
      .map(p => point(p.r - 1, p.c - 1)) // shift grid up/left so that 0,0 is in-bounds

    val result = runCourse(point(0, 0), point(0, 19), maze)
    assert(result.isDefined)
    assert(result.get.costToReach >= 30 && result.get.costToReach <= 33)

    val result2 = runCourse(point(0, 0), point(9, 4), maze)
    assert(result2.isDefined)
    assert(result2.get.costToReach == 35)

  }

  def fivebyfiveTests() = {
    val rawMaze = List(
      // 01234
      /* * */ "+-----+",
      /* 0 */ "|.###.|",
      /* 1 */ "|..##.|",
      /* 2 */ "|.#...|",
      /* 3 */ "|...##|",
      /* 4 */ "|####.|",
      /* * */ "+-----+"
    )
    val maze = parseGrid(rawMaze)
      .map(p => point(p.r - 1, p.c - 1)) // shift grid up/left so that 0,0 is in-bounds

    val goodResult = runCourse(point(0, 0), point(2, 4), maze)
    assert(goodResult.isDefined)
    assert(goodResult.get.costToReach == 8)

    val badResult = runCourse(point(0, 0), point(4, 4), maze)
    assert(badResult.isEmpty)
  }

  def point(row: Int, col: Int) = TwoDPoint.rc(row, col)
  def runCourse(
      start: TwoDPoint,
      end: TwoDPoint,
      maze: List[TwoDPoint]
  ): Option[Step[TwoDPoint]] = {
    val startStep = Step(start, 0, start.manhattanDistance(end))
    var seen = ArrayBuffer[Step[TwoDPoint]]()
    var nexts = new MinHeap[Step[TwoDPoint]]()
    Helpers.aStarHeap(startStep, end, maze, seen, nexts, TwoDPoint.manhattanDistance)
  }
}

object Helpers {

  @tailrec
  def aStarHeap[T <: HasAdjacencies[T]](
      origin: Step[T],
      target: T,
      walls: List[T],
      seen: ArrayBuffer[Step[T]],
      possibleSteps: MinHeap[Step[T]],
      distFn: (T, T) => Int
  ): Option[Step[T]] = {
    if (origin.node.equals(target)) {
      Some(origin)
    } else {
      // 1. find adjacencies + filter out unreachable nodes
      val validMovement = origin.node.adjacencies.diff(walls)
      val nextSteps = validMovement
        .map(n => Step(n, origin.costToReach + 1, distFn(n, target)))

      // 2. update seen list (add new point, remove old points if better path found)
      seen += origin

      val toAdd = ArrayBuffer[Step[T]]()
      nextSteps.foreach( step => {
        val index = seen.indexWhere(s => s.node == step.node)
        if (index == -1) {
          toAdd.append(step)
        } else if (seen(index).costToReach > step.costToReach) {
          toAdd.append(step)
          seen(index) = seen(seen.length - 1)
          seen.remove(seen.length-1)
        }
      })

      toAdd.foreach(step => {
        val position = possibleSteps.find(s => s.node == step.node)
        if (position > -1) {
          val knownStep = possibleSteps.peek(position)
          if (knownStep.costToGoalEstimate > step.costToGoalEstimate) {
            possibleSteps.update(step, position)
          }
        } else {
          possibleSteps.addNode(step)
        }
      })

      // 3. update possible steps
      if (possibleSteps.isEmpty) {
        None
      } else {
        // 4. find the next best step
        val nextStep = possibleSteps.delete()

        aStarHeap(nextStep, target, walls, seen, possibleSteps, distFn)
      }
    }
  }

// //////////////
  // @tailrec
  // def aStar[T <: HasAdjacencies[T]](
  //     origin: Step[T],
  //     target: T,
  //     walls: List[T],
  //     seen: ListBuffer[Step[T]],
  //     possibleSteps: ListBuffer[Step[T]],
  //     distFn: (T, T) => Int
  // ): Option[Step[T]] = {
  //   if (origin.node.equals(target)) {
  //     Some(origin)
  //   } else {
  //     // 1. find adjacencies + filter out unreachable nodes
  //     val validMovement = origin.node.adjacencies.diff(walls)
  //     val nextSteps = validMovement
  //       .map(n => Step(n, origin.costToReach + 1, distFn(n, target)))

  //     // 2. update seen list (add new point, remove old points if better path found)
  //     seen += origin
  //     val toAdd = nextSteps
  //       .map(step => {
  //         val index = seen.indexWhere(s => s.node == step.node)
  //         index match {
  //           case -1 => Some(step)
  //           case _ => {
  //             if (seen(index).costToReach > step.costToReach) {
  //               seen(index).costToReach = step.costToReach
  //               Some(seen.remove(index))
  //             } else None
  //           }
  //         }
  //       })
  //       .filter(_.isDefined)
  //       .map(_.get)
  //       .map(step => {
  //         val index = possibleSteps.indexWhere(s => s.node == step.node)
  //         index match {
  //           case -1 => Some(step)
  //           case _ => {
  //             if (possibleSteps(index).costToReach > step.costToReach) {
  //               possibleSteps(index).costToReach = step.costToReach
  //               Some(possibleSteps.remove(index))
  //             } else None
  //           }
  //         }
  //       })
  //       .filter(_.isDefined)
  //       .map(_.get)

  //     // 3. update possible steps
  //     possibleSteps.appendAll(toAdd)
  //     if (possibleSteps.length == 0) {
  //       None
  //     } else {
  //       // 4. find the next best step
  //       var minStepIndex = 0
  //       var minCost      = possibleSteps.head.totalCost()
  //       for (step <- possibleSteps.tail.zipWithIndex) {
  //         if (step._1.totalCost() < minCost) {
  //           minStepIndex = step._2
  //           minCost = step._1.totalCost()
  //         }
  //       }
  //       val nextStep = possibleSteps.remove(minStepIndex)

  //       aStar(nextStep, target, walls, seen, possibleSteps, distFn)
  //     }
  //   }
  // }


}

case class Step[T](node: T, var costToReach: Int, costToGoalEstimate: Int)
    extends Ordered[Step[T]] {
  def totalCost(): Int = {
    costToReach + costToGoalEstimate
  }
  def nodeEquals(node: T) = {
    this.node == node
  }
  def update(newCost: Int): Step[T] = {
    Step(node, math.min(newCost, costToReach), costToGoalEstimate)
  }

  def compare(that: Step[T]): Int = {
    java.lang.Integer.compare(totalCost, that.totalCost)
  }
}

case class TwoDPoint(x: Int, y: Int) extends HasAdjacencies[TwoDPoint] {
  def isAdjacent(that: TwoDPoint) = manhattanDistance(that) == 1

  def adjacencies() = {
    Array[TwoDPoint](
      TwoDPoint(this.x + 1, this.y),
      TwoDPoint(this.x - 1, this.y),
      TwoDPoint(this.x, this.y + 1),
      TwoDPoint(this.x, this.y - 1)
    )
  }

  def manhattanDistance(p: TwoDPoint): Int = {
    val xDiff = math.abs(this.x - p.x)
    val yDiff = math.abs(this.y - p.y)
    (xDiff + yDiff)
  }
  def r(): Int = y
  def c(): Int = x
}

object TwoDPoint {

  /** gridInterior produces an _exterior_ grid (points only) that are strictly outside of the bounds
    * of the grid. i.e. specifying (1, 1) will produce a grid with 1 free space, and 8 walls
    * surrouding it
    */
  def gridInterior(rows: Int, cols: Int) = {
    val top    = wall(-1, (-1 to cols))
    val bottom = wall(rows, (-1 to cols))
    val left   = wall((0 until rows), -1)
    val right  = wall((0 until rows), cols)
    top ++ bottom ++ left ++ right
  }

  def wall(row: Int, colRange: Range): List[TwoDPoint] = {
    colRange.map(cI => TwoDPoint(cI, row)).toList
  }

  def wall(rowRange: Range, col: Int): List[TwoDPoint] = {
    rowRange.map(rI => TwoDPoint(col, rI)).toList
  }

  def rc(row: Int, col: Int): TwoDPoint = {
    TwoDPoint(col, row)
  }

  def manhattanDistance(a: TwoDPoint, b: TwoDPoint): Int = {
    val xDiff = math.abs(a.x - b.x)
    val yDiff = math.abs(a.y - b.y)
    (xDiff + yDiff)
  }

  def renderGrid(walls: List[TwoDPoint], wallChar: Char = '#', emptyChar: Char = '.'): String = {
    val minCol = walls.minBy(p => p.x).x
    val maxCol = walls.maxBy(p => p.x).x
    val minRow = walls.minBy(p => p.y).y
    val maxRow = walls.maxBy(p => p.y).y

    (minRow to maxRow)
      .map(rI => {
        (minCol to maxCol)
          .map(cI => {
            val found = walls.find(w => w == TwoDPoint.rc(rI, cI))
            found match {
              case None        => emptyChar
              case Some(value) => wallChar
            }
          })
          .mkString
      })
      .mkString("\n")
  }
}

case class ThreeDPoint(x: Int, y: Int, z: Int) {
  def isAdjacent(that: ThreeDPoint) = {
    manhattanDistance(that) == 1
  }

  def adjacencies() = {
    Array[ThreeDPoint](
      ThreeDPoint(this.x + 1, this.y, this.z),
      ThreeDPoint(this.x - 1, this.y, this.z),
      ThreeDPoint(this.x, this.y + 1, this.z),
      ThreeDPoint(this.x, this.y - 1, this.z),
      ThreeDPoint(this.x, this.y, this.z + 1),
      ThreeDPoint(this.x, this.y, this.z - 1)
    )
  }

  def manhattanDistance(p: ThreeDPoint): Int = {
    val xDiff = math.abs(this.x - p.x)
    val yDiff = math.abs(this.y - p.y)
    val zDiff = math.abs(this.z - p.z)
    (xDiff + yDiff + zDiff)
  }
}
