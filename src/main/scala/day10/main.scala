package day10

import scala.io.Source
import Ticker


object Main {
  def main(args: Array[String]) = {
    val cmdFeed = Source.fromResource("day10_input.txt").getLines()

    val partFeeds = cmdFeed.duplicate
    
    var signalSum = new Ticker(partFeeds._1)
      .zipWithIndex
      .map(entry => (entry._1, entry._2+1)) // fix tick vs index (1 based vs 0 based)
      .filter(entry => entry._2 == 20 || ((entry._2 - 20) % 40 == 0))
      .map(entry => entry._2 * entry._1)
      .sum
    println(s"Part 1: Sum of signal samples: $signalSum")

    
  }
}
