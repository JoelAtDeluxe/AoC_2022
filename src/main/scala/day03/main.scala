package day03

import scala.io.Source

object Main {
  def main(args: Array[String]) = {
    val lines = Source
      .fromResource("day03_input.txt")
      .getLines()
      .toList
    
    // val rucksackPriority = partOne(lines)
    val rucksackPriority = prioritizeRucksack(lines, _.map(divide))
    println(s"Part 1: Combined rucksack priority is: $rucksackPriority")

    // val groupedPriorities = partTwo(lines)
    val groupedPriorities = prioritizeRucksack(lines, _.grouped(3).toList)
    println(s"Part 2: Combined grouped priority is: $groupedPriorities")

  }

  def thing(l: List[String]) ={
    val grouped = l.grouped(3).toList
    val split = l.map(divide)
  }

  def prioritizeRucksack(lines: List[String], fn: (List[String] => List[List[String]])) = {
    val priority = fn(lines)
      .map(determineIntersection)
      .map(prioritize)
      .sum

    priority
  }

  def determineIntersection(l: Seq[String]): String = {
    l
    .map(_.toSet)
    .reduce( (a, b) => a.intersect(b) )
    .mkString
  }

  def divide(l: String): List[String] = {
    val paritionOn = l.length() / 2
    return List(
      l.slice(0, paritionOn),
      l.slice(paritionOn, l.length())
    )
  }

  def prioritize(s: String): Int = {
    val lowerOffset = 'a'.toInt - 1
    val upperOffset = 'A'.toInt - 1
    s.map(ch => {
      val chVal = ch.toInt
      if (between(ch, 'a', 'z'))
        chVal - lowerOffset
      else
        chVal - upperOffset + 26
    }).sum
  }

  def between(ch: Char, lower: Char, upper: Char): Boolean = {
    val bounds = List(lower, upper).sorted
    bounds(0) <= ch && ch <= bounds(1)
  }
}
