package day14

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec
import ujson.Arr

object Main {
  val ORIGIN: Int = -1
  val OPEN: Int   = 0
  val WALL: Int   = 1
  val SAND: Int   = 2

  def main(args: Array[String]) = {
    val shapes = Source
      .fromResource("days11_20/day14_input.txt")
      .getLines()
      .map(parseInputLine)
      .toList

    val source       = Point(500, 0)
    val shapedBounds = findBounds(Shape(List(source)) +: shapes)

    val partOneDrops = solveSandDrop(source, shapes, Some(shapedBounds))
    println(s"Part 1: number of drops: ${partOneDrops}")

    val (tl, br) = shapedBounds
    val height = br.y - tl.y
    
    // worst case (mem) is +/- height, but it could be smaller
    val ground   = Shape(List(
      Point(tl.x - height, br.y + 2),
      Point(br.x + height, br.y + 2)
    ))

    val partTwoDrops = solveSandDrop(source, ground +: shapes, None)
    println(s"Part 2: number of drops: ${partTwoDrops}")

  }

  def solveSandDrop(source: Point, shapes: List[Shape], bounds: Option[(Point, Point)]): Int = {
    val (tl, br) = if(bounds.isDefined) bounds.get else findBounds(Shape(List(source)) +: shapes)

    val offsetSource = Point(source.x - tl.x, source.y - tl.y)
    val grid         = populateGrid(shapes, tl, br)
    grid(source.y - tl.y)(source.x - tl.x) = ORIGIN

    simulate(grid, offsetSource, 1)
  }

  @tailrec
  def simulate(grid: Grid, source: Point, count: Int): Int = {
    if (at(grid, source) == SAND) {
      println("done!")
      count - 1
    } else {
      val restingPoint = dropSand(source, grid)

      if (restingPoint.isDefined) {
        grid(restingPoint.get.y)(restingPoint.get.x) = SAND
        simulate(grid, source, count + 1)
      } else {
        count - 1 // always drops one too many --that's how we know to stop
      }
    }
  }

  def canMove(origin: Point, grid: Grid): Option[Boolean] = {
    if (!withinGrid(grid, origin)) {
      None
    } else if (at(grid, origin) == OPEN) {
      Some(true)
    } else { // wall or sand
      Some(false)
    }
  }

  @tailrec
  def checkMoves(grid: Grid, spots: Array[Point], index: Int): Option[Int] = {
    if (index >= spots.length) {
      Some(index)
    } else {
      val point = spots(index)
      canMove(point, grid) match {
        case Some(true)  => Some(index)
        case Some(false) => checkMoves(grid, spots, index + 1)
        case None        => None
      }
    }
  }

  @tailrec
  def dropSand(origin: Point, grid: Grid): Option[Point] = {
    val down      = Point(origin.x, origin.y + 1)
    val downLeft  = Point(origin.x - 1, origin.y + 1)
    val downRight = Point(origin.x + 1, origin.y + 1)
    val options   = Array(down, downLeft, downRight)

    val result = checkMoves(grid, options, 0)
    result match {
      case None                          => None
      case Some(v) if v < options.length => dropSand(options(v), grid)
      case _                             => Some(origin)
    }
  }

  def parseInputLine(line: String): Shape = {
    val points = line
      .split("->")
      .map(_.trim.split(","))
      .map(pair => pair.map(_.toInt))
      .map(pair => Point(pair(0), pair(1)))
      .toList

    Shape(points)
  }

  def findBounds(shapes: List[Shape]): (Point, Point) = {
    var topLeft     = Point(999, 999)
    var bottomRight = Point(0, 0)

    for (shape <- shapes) {
      for (p <- shape.path) {
        val left = if (p.x < topLeft.x) p.x else topLeft.x
        val top  = if (p.y < topLeft.y) p.y else topLeft.y

        val right  = if (p.x > bottomRight.x) p.x else bottomRight.x
        val bottom = if (p.y > bottomRight.y) p.y else bottomRight.y
        topLeft = Point(left, top)
        bottomRight = Point(right, bottom)
      }
    }

    (topLeft, bottomRight)
  }

  def populateGrid(shapes: List[Shape], topLeft: Point, bottomRight: Point): Grid = {
    // reversing height and width / y and x so that it matches my expectations
    val width  = bottomRight.x - topLeft.x + 1
    val height = bottomRight.y - topLeft.y + 1
    val grid   = Array.ofDim[Int](height, width)

    shapes.foreach(s => {
      var firstPoint = s.path(0)
      for (nextPoint <- s.path.tail) {
        fillGaps(firstPoint, nextPoint).foreach(p => {
          grid(p.y - topLeft.y)(p.x - topLeft.x) = 1
        })
        firstPoint = nextPoint
      }
    })

    grid
  }

  def renderGrid(grid: Grid): String = {
    grid
      .map(row => {
        row
          .map(_ match {
            case ORIGIN => '+'
            case WALL   => '#'
            case SAND   => 'o'
            case _      => '.'
          })
          .mkString
      })
      .mkString("\n")
  }

  type Grid = Array[Array[Int]]

  def at(grid: Grid, point: Point): Int = {
    grid(point.y)(point.x)
  }
  def withinGrid(grid: Grid, point: Point): Boolean = {
    if (
      point.y >= 0 && point.y < grid.length &&
      point.x >= 0 && point.x < grid(0).length
    ) true
    else false
  }

  def fillGaps(p1: Point, p2: Point): List[Point] = {
    val left = p2.x - p1.x
    val top  = p2.y - p1.y
    val (qty, incPoint) = if (left == 0) {
      (top, Point(0, math.signum(top)))
    } else {
      (left, Point(math.signum(left), 0))
    }

    (0 to math.abs(qty))
      .map(idx => incPoint.scale(idx))
      .map(stepAmt => p1.translate(stepAmt))
      .toList
  }

}

case class Point(x: Int, y: Int) {
  def scale(qty: Int) = {
    Point(x * qty, y * qty)
  }
  def translate(p: Point) = {
    Point(x + p.x, y + p.y)
  }
}

case class Shape(path: List[Point])
