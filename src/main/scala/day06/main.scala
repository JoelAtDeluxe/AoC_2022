package day06

import scala.io.Source
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    // solvePracticeProblems()

    val data          = Source.fromResource("day06_input.txt").mkString
    val startOfSignal = solve(4, data, 4)

    startOfSignal match {
      case Some(v) => {
        println(s"Part 1: Signal starts at: ${v}")
        val startOfMessage = solve(v, data, 14)
        startOfMessage match {
          case Some(msgStart) => println(s"Part 1: Signal starts at: ${msgStart}")
          case None           => println("Unable to find start of Message")
        }
      }
      case _ => println("Unable to find start of signal")
    }
  }

  @tailrec
  def solve(index: Int, message: String, startOfSignalLength: Int): Option[Int] = {
    val EXIT    = -2
    val RECURSE = -1
    val result = index match {
      case idx if idx > message.length()    => EXIT
      case idx if idx < startOfSignalLength => RECURSE
      case idx => {
        val numUniqueChars = message.slice(idx - startOfSignalLength, idx).toSet.size
        numUniqueChars match {
          case num if num < startOfSignalLength => RECURSE
          case _                                => idx
        }
      }
    }

    result match {
      case EXIT    => None
      case RECURSE => solve(index + 1, message, startOfSignalLength)
      case n       => Some(n)
    }
  }

  def solvePracticeProblems() = {
    val sampleData = loadSampleInput("day06_merged_sample.txt")

    for (entry <- sampleData) {
      val result = solve(0, entry._3, 4)
      println(s"P1: Answer should be: ${entry._1} but got: ${result}")
      val result2 = solve(0, entry._3, 14)
      println(s"P2: Answer should be: ${entry._2} but got: ${result2}")
    }
  }

  def loadSampleInput(name: String): List[(Int, Int, String)] = {
    Source
      .fromResource(name)
      .getLines()
      .map(_.split(" +", 3))
      .map(a => (a(0).toInt, a(1).toInt, a(2)))
      .toList
  }
}
