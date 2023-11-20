package day09

import scala.io.Source
import scala.collection.mutable
import scala.collection.immutable.HashSet
import {Up, Right, Direction, Down, Left}

object Main {

  def main(args: Array[String]) = {
    val stepFeed = Source
      .fromResource("day09_input.txt")
      .getLines()

    // drawAndSolve(stepFeed)
    val junk = traceTail(stepFeed)
    println(s"Part 1: ${junk.size}")
  }

  def traceTail(stepper: Iterator[String]) = {
    val headStartPos = (0, 0)
    val tailStartPos = (0, 0)

    val visited = new HashSet[(Int, Int)]()
    val finalVisisted = stepper.foldLeft( (visited + tailStartPos, headStartPos, tailStartPos))( (acc, cur) => {
      val parts = cur.split(" ")

      var (currentVisisted, currentHead, currentTail) = acc

      val movement = parts(1).toInt
      val direction = parts(0) match {
        case "R" => Right()
        case "L" => Left()
        case "U" => Up()
        case "D" => Down()
      }

      (0 until movement).foreach( _ => {
        val result = performStep(direction, currentHead, currentTail)
        currentHead = result._1
        currentTail = result._2
        currentVisisted = currentVisisted + currentTail
      })
      (currentVisisted, currentHead, currentTail)
    })

    finalVisisted._1
  }

  def performStep(direction: Direction, headPos: (Int, Int), tailPos: (Int, Int)): ((Int, Int), (Int, Int)) = {
    val newHead = direction match {
      case _: Up => (headPos._1 - 1, headPos._2)
      case _: Down => (headPos._1 + 1, headPos._2)
      case _: Left => (headPos._1, headPos._2 - 1)
      case _: Right => (headPos._1, headPos._2 + 1)
    }

    val newTail = moveTail(newHead, tailPos)
    (newHead, newTail)
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

  def drawAndSolve(stepper: Iterator[String]) = {
    val visited = mutable.Set[(Int, Int)]()
    var headPos = (0, 0)
    var tailPos = (0, 0)
    visited.add(tailPos)

    for (row <- stepper) {
      val parts = row.split(" ")
      val movement = parts(1).toInt
      val direction = parts(0) match {
        case "R" => Right()
        case "L" => Left()
        case "U" => Down() // Up()
        case "D" => Up() //   Down()
      }

      for (_ <- 0 until movement) {
        val result = performStep(direction, headPos, tailPos)
        headPos = result._1
        tailPos = result._2
        visited.add(tailPos)

        for(r <- 4 to 0 by -1) {
          for(c <- 0 until 5) {
            val ch = (r, c) match {
              case v if v == headPos => "H"
              case v if v == tailPos => "T"
              case v if visited.contains(v) => "#"
              case _ => "."
            }
            print(ch)
          }
          println()
        }
        println()
      }
    }
    println(s"Total spots tail visisted: ${visited.size}")
  }
}
