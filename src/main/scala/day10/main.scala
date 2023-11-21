package day10

import scala.io.Source

object Main {
  def main(args: Array[String]) = {
    val cmdFeed = Source.fromResource("day10_input.txt").getLines()

    var signalStrengthLog = List[Int]()
    val shouldSample = (tickCount: Int) => {
      tickCount == 20 || ((tickCount - 20)%40 == 0)
    }
    process(cmdFeed, (tickCount, xCell) => {
      val sigStrength = if (shouldSample(tickCount)) {
        tickCount * xCell
      } else 0
      signalStrengthLog = sigStrength +: signalStrengthLog
    })
    val realLog = signalStrengthLog.filter(x => x > 0).reverse
    val signalSum = realLog.sum
    println(s"Part 1: Sum of signal samples: $signalSum")
    // realLog.foreach(println)
  }

  // TODO try iterator approach

  def process(feed: Iterator[String], onTick: (Int, Int) => Unit) = {
    var xCell = 1
    var ticks = 0
    feed.foreach(cmd => {
      val action = cmd match {
        case "noop" => Noop()
        case line => AddX(line.split(" ", 2)(1).toInt) 
      }
      (1 until action.ticks+1).foreach(tickItr => onTick(ticks + tickItr, xCell))
      val xIncrease = action match {
        case AddX(v) => v
        case Noop() => 0
      } 
      xCell = xCell + xIncrease
      ticks = ticks + action.ticks
    })
  }
}
