package day12

import scala.io.Source
import scala.annotation.tailrec
import scala.collection.mutable

object Main {
  def main(args: Array[String]) = {
    val lines = Source.fromResource("days11_20/day12_input.txt").getLines.toList
    // find starting point and ending point

    val (start, end) = findStartEnd(lines, 0, Coordinate(), Coordinate())

    val updatedMap = readyGraph(lines, start, end)

    // try {
    //   stagedWalk(updatedMap, start, 'p')
    // }
    // finally {
    //   // (Seq("sh", "-c", "stty cooked </dev/tty") !)
    // }

    walk(updatedMap, start, end)

    println("Done")
  }

  def bold(s: String) : String  = { s"\033[1m$s\033[21m" }
  def inverted(s: String) : String  = { s"\033[7m$s\033[27m" }
  def red(s: String): String    = { s"\033[31m$s\033[39m" }
  def yellow(s: String): String = { s"\033[93m$s\033[39m" }
  def blue(s: String): String   = { s"\033[34m$s\033[39m" }


  @tailrec
  def stagedWalk(grid: Grid[Int], coord: Coordinate, lastChar: Char):Unit = {
    print("\033[2J")

    println(grid.renderBetter(grid.intToStr, Map{
      coord -> inverted(bold(red(grid.intToStr(grid.at(coord)))))
    }))
    val ch = scala.io.StdIn.readChar()
    
    ch match {
      case 'h' => stagedWalk(grid, coord.left, ch)
      case 'j' => stagedWalk(grid, coord.down, ch)
      case 'k' => stagedWalk(grid, coord.up, ch)
      case 'l' => stagedWalk(grid, coord.right, ch)
      case 'p' => {}
      case _ => stagedWalk(grid, coord, ch)
    }
  }


  def walk(
      grid: Grid[Int],
      start: Coordinate,
      end: Coordinate
  ) = {
    var seenNodes: List[CellScore] = List()
    val openSet = new MinHeap[CellScore]()
    openSet.addNode(CellScore(start, 0, distance(grid, start, end)))

    val gScores = mutable.Map[Coordinate, Int]()
    gScores.put(start, 0)
    val fScores = mutable.Map[Coordinate, Int]()
    fScores.put(start, distance(grid, start, end))

    var stop = false
    while (!stop) {
      println(s"OpenSet: ${openSet.render()}")
      val current = openSet.delete()
      println(current.coord)
      seenNodes = current +: seenNodes

      if (current.coord == end) {
        stop = true
        // we won! // TODO
      } else {
        List(current.coord.up, current.coord.down, current.coord.left, current.coord.right)
          .filter(grid.contains) // check in bounds
          .filter(c => vDistance(grid, current.coord, c) <= 1) // check height
          .foreach(neighbor => {
            // println(s"  ?! ${neighbor}")
            val stepDistance    = 1 // hDistance(grid, current.coord, neighbor)
            val tentativeGScore = current.cost + stepDistance

            val neighIndex = openSet.find(cs => cs.coord == neighbor)
            if (neighIndex > -1) {
              var oldValue = openSet.backing(neighIndex)
              if (tentativeGScore < oldValue.cost) {
                var newVal = CellScore(oldValue.coord, tentativeGScore, oldValue.distance)
                openSet.update(newVal, neighIndex)
              }
            } else {
              openSet.addNode(score(grid, neighbor, end, tentativeGScore))
            }
          })
      }
    }

    var closed = List[CellScore]()
  }

  def hDistance(grid: Grid[Int], c1: Coordinate, c2: Coordinate) = {
    val rowDistance = Math.abs(c1.row - c2.row)
    val colDistance = Math.abs(c1.col - c2.col)
    rowDistance + colDistance
  }

  def distance(grid: Grid[Int], c1: Coordinate, c2: Coordinate) = {
    var heightDistance = vDistance(grid, c1, c2)
    // heightDistance = if (heightDistance > 1) 9999 else heightDistance
    val planeDistance  = hDistance(grid, c1, c2)
    heightDistance + planeDistance
  }

  def vDistance(grid: Grid[Int], c1: Coordinate, c2: Coordinate) = {
    Math.max(grid.at(c2) - grid.at(c1), 0)
  }

  def score(grid: Grid[Int], start: Coordinate, end: Coordinate, parentFullCost: Int): CellScore = {
    val dist = distance(grid, start, end)
    CellScore(start, parentFullCost, dist)
  }

  def readyGraph(map: List[String], start: Coordinate, end: Coordinate): Grid[Int] = {
    // replace start and end
    val heightMap = map.zipWithIndex
      .map(entry => {
        val lineNumber = entry._2

        val replaceStr = if (lineNumber == start.row) { entry._1.replace('S', 'a') }
        else { entry._1 }
        val finalStr = if (lineNumber == end.row) { replaceStr.replace('E', 'z') }
        else { replaceStr }

        finalStr.map(ch => ch.toInt - 96).toArray
      })
      .toArray

    Grid(heightMap)
  }

  @tailrec
  def findStartEnd(
      map: List[String],
      lineNum: Int,
      startPoint: Coordinate,
      endPoint: Coordinate
  ): (Coordinate, Coordinate) = {
    if (lineNum > map.length || (startPoint.isSet && endPoint.isSet)) {
      (startPoint, endPoint)
    } else {
      val line     = map(lineNum)
      val endCol   = line.indexOf("E")
      val startCol = line.indexOf("S")
      val endPosition = if (endCol > -1) { Coordinate(lineNum, endCol) }
      else endPoint
      val startPosition = if (startCol > -1) { Coordinate(lineNum, startCol) }
      else startPoint

      findStartEnd(map, lineNum + 1, startPosition, endPosition)
    }
  }
}
