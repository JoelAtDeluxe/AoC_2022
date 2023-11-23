package day04

import scala.io.Source

object Main {
  def main(args: Array[String]) = {
    val ranges = Source
      .fromResource("days01_10/day04_input.txt")
      .getLines()
      .map(lineToRange)
      .toList

    val perfectOverlaps = ranges
      .map(parts => containedIn(parts(0), parts(1)))
      .count(x=>x)

    println(s"Part 1: There are: ${perfectOverlaps} perfect overlaps")

    val partialOverlaps = ranges
      .map(parts => partiallyContainedIn(parts(0), parts(1)))
      .count(x=>x)

    println(s"Part 2: There are: ${partialOverlaps} partial overlaps")

  }

  def lineToRange(line: String): List[List[Int]] = {
    line
      .split(",") // separate elves
      .map(
        _.split("-") // separate range ends
          .map(_.toInt)
          .toList
      )
      .toList
  }

  def containedIn(first: List[Int], second: List[Int]): Boolean = {
    val contains = (a: List[Int], b:List[Int]) => a(0) <= b(0) && a(1) >= b(1)
    contains(first, second) || contains(second, first)
  }

  def partiallyContainedIn(first: List[Int], second: List[Int]): Boolean = {
    val between = (lower: Int, upper: Int, test: Int) => {
      (lower <= test) && (test <= upper)
    }

    val f = between(first(0), first(1), second(0))
    val o = between(first(0), first(1), second(1))
    val i = between(second(0), second(1), first(0))
    val l = between(second(0), second(1), first(1))

    f || o || i || l
  }

}
