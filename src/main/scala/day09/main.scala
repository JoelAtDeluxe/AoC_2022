package day09

import scala.io.Source
import scala.collection.mutable
import scala.collection.immutable.HashSet

object Main {

  def main(args: Array[String]) = {
    val stepFeed = Source
      .fromResource("day09_input.txt")
      .getLines()

    val simpleRope = List((0,0), (0,0))
    val longRope = List(
      (0, 0), (0, 0), (0, 0),
      (0, 0), (0, 0), (0, 0),
      (0, 0), (0, 0), (0, 0),
      (0, 0))

    // drawAndSolve(stepFeed, longRope, 26, (9, 11))
    val splitFeed = stepFeed.duplicate
    var visitedSet = traceTail(splitFeed._1, simpleRope)
    println(s"Part 1: short rope visited: ${visitedSet.size}")

    visitedSet = traceTail(splitFeed._2, longRope)
    println(s"Part 2: long rope visited ${visitedSet.size}")

  }

  def traceTail(stepper: Iterator[String], initialRope: List[(Int, Int)]) = {
    val visited = new HashSet[(Int, Int)]()
    val finalVisisted = stepper.foldLeft( (visited + initialRope.last, initialRope))( (acc, cur) => {
      val parts = cur.split(" ")

      var (currentVisisted, currentRope) = acc

      val movement = parts(1).toInt
      val direction = parts(0) match {
        case "R" => Right()
        case "L" => Left()
        case "U" => Up()
        case "D" => Down()
      }

      (0 until movement).foreach( _ => {
        val result = performStep(direction, currentRope)
        currentRope = result
        currentVisisted = currentVisisted + currentRope.last
      })
      (currentVisisted, currentRope)
    })

    finalVisisted._1
  }

  def performStep(direction: Direction, ropeSegments: List[(Int, Int)]): List[(Int, Int)] = {
    val headPos = ropeSegments.head
    val newHead = direction match {
      case _: Up => (headPos._1 - 1, headPos._2)
      case _: Down => (headPos._1 + 1, headPos._2)
      case _: Left => (headPos._1, headPos._2 - 1)
      case _: Right => (headPos._1, headPos._2 + 1)
    }

    var segmentHead = newHead
    val newTail = ropeSegments.tail.map( tailSegment => {
      val newSegmentTail = moveTail(segmentHead, tailSegment)
      segmentHead = newSegmentTail
      newSegmentTail
    })
    
    newHead +: newTail
  }

  def moveTail(headPos: (Int, Int), tailPos: (Int, Int)): (Int, Int) = {
    val fGap = headPos._1 - tailPos._1
    val lGap = headPos._2 - tailPos._2

    //   $0$
    //  $---$
    //  0-!-0
    //  $---$
    //   $0$

    (fGap, lGap) match {
      // ordinals
      case (-2,  0) => (tailPos._1 - 1, tailPos._2) // up
      case ( 0, -2) => (tailPos._1, tailPos._2 - 1) // left
      case ( 2,  0) => (tailPos._1 + 1, tailPos._2) // down
      case ( 0,  2) => (tailPos._1, tailPos._2 + 1) // right

      // up lefts
      // case (-2, -1) => ??? // up left
      // case (-1, -2) => ??? // left up
      case (-2, -1) | (-1, -2) => (tailPos._1 -1, tailPos._2 - 1)

      // up rights
      // case (-2,  1) => ??? // up right
      // case (-1,  2) => ??? // right up
      case (-2,  1) | (-1,  2) => (tailPos._1 -1, tailPos._2 + 1) // right up

      // down rights
      // case ( 2,  1) => ??? // Down right
      // case ( 1,  2) => ??? // right Down
      case ( 2,  1) | ( 1,  2) => (tailPos._1 + 1, tailPos._2 + 1) // right Down


      // down left
      // case ( 2, -1) => ??? // down left
      // case ( 1, -2) => ??? // left down
      case ( 2, -1) | ( 1, -2) => (tailPos._1 + 1, tailPos._2 - 1) // left down
      case _ => tailPos
    }
  }

  def drawAndSolve(
    stepper: Iterator[String],
    initialRope: List[(Int, Int)],
    gridSize: Int=5,
    start: (Int, Int)=(0,0)
    ) = {
    val visited = mutable.Set[(Int, Int)]()
    var rope = initialRope.map(_ => start)
    visited.add(rope.last)

    for (row <- stepper) {
      val parts = row.split(" ")
      val movement = parts(1).toInt
      val direction = parts(0) match {
        case "R" => Right()
        case "L" => Left()
        // swap to make drawing look like example
        case "U" => Down() // Up()
        case "D" => Up() //   Down()
      }

      for (_ <- 0 until movement) {
        rope = performStep(direction, rope)
        visited.add(rope.last)

        for(r <- (gridSize-1) to 0 by -1) {
          for(c <- 0 until gridSize) {
            val ch = (r, c) match {
              case v if v == rope.head => "H"
              case v if rope.indexOf(v) > -1 => rope.indexOf(v)
              case v if v == rope.last => "T"
              case v if visited.contains(v) => "#"
              case _ => "."
            }
            print(ch)
          }
          println()
        }
        printRope(rope)
        println()

      }
    }
    println(s"Total spots tail visisted: ${visited.size}")
  }

  def printRope(rope: List[(Int, Int)]) = {
    println(rope.map(seg => s"(${seg._1}, ${seg._2})").mkString(" -> "))

  }
}
