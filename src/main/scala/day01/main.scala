package day01

import scala.io.Source

object Main {
  def main(args: Array[String]) = {
    val reader = Source.fromResource("days01_10/day01_input.txt")

    val elfFood = reader.mkString // as a string
      .split("\n\n")                        // split on empty lines
      .map(_.split("\n").map(_.trim.toInt)) // split on lines + turn to int
      .map(_.sum)                           // sum the array generated above

    val mostCals = elfFood
      .reduce((a, b) => Math.max(a, b))

    println(s"Part 1 Most Carried by single Elf: ${mostCals}")

    // slow
    val sorted = elfFood.sorted(Ordering[Int].reverse)
    println(s"Part 2: Sum of top three elves: ${sorted.slice(0, 3).sum}")

    // faster?
    val thing = elfFood
      .foldLeft(List(0, 0, 0))((acc, cur) => {

        val biggerThanIndex = acc.indexWhere(a => cur > a)
        if (biggerThanIndex == -1)
          acc
        else
          (acc.slice(0, biggerThanIndex) :+ cur) ++ acc
            .slice(biggerThanIndex + 1, acc.length)

      })
      .sum

    println(s"Part 2b: Sum of top three elves: ${thing}")
  }
}
