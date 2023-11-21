package day10

import scala.io.Source
import scala.collection.mutable.Queue

object Main {
  def main(args: Array[String]) = {
    val cmdFeed = Source.fromResource("day10_input.txt").getLines()

    // old part 1
    // var signalStrengthLog = List[Int]()
    // val shouldSample = (tickCount: Int) => {
    //   tickCount == 20 || ((tickCount - 20)%40 == 0)
    // }
    // process(cmdFeed, (tickCount, xCell) => {
    //   val sigStrength = if (shouldSample(tickCount)) {
    //     tickCount * xCell
    //   } else 0
    //   signalStrengthLog = sigStrength +: signalStrengthLog
    // })
    // val realLog = signalStrengthLog.filter(x => x > 0).reverse
    // val signalSum = realLog.sum
    // println(s"Part 1: Sum of signal samples: $signalSum")
    // // realLog.foreach(println)

    val ticker = new Ticker(cmdFeed)
    var signalSum = ticker
      .zipWithIndex
      .map(entry => (entry._1, entry._2+1)) // fix tick vs index (1 based vs 0 based)
      .filter(entry => entry._2 == 20 || ((entry._2 - 20) % 40 == 0))
      .map(entry => entry._1.tick * entry._1.cell)
      .sum
      // .foldLeft((0, 1))((acc, cur) => {
        
      // })
    println(s"Part 1: Sum of signal samples: $signalSum")
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

class Ticker(val source: Iterator[String]) extends Iterator[MachineState] {
  var cellValue = 1
  var tick = 0
  val actionQueue: Queue[Action] = Queue()

  def hasNext: Boolean = {
    actionQueue.size > 0 || source.hasNext
  }

  def next(): MachineState = {
    if (actionQueue.isEmpty) { // pull the next element
      if (!source.hasNext) {
        throw new Exception("Stop iterating!")
      }
      val rawEvent = source.next()
      val action = rawEvent match {
        case "noop" => Noop()
        case line => AddX(line.split(" ", 2)(1).toInt) 
      }
      // ignore tick timer -- just add on empty steps
      val bufferActions = (0 until action.ticks - 1)
        .map(_=>Noop()) :+ action

      actionQueue.enqueue(bufferActions:_*)
    }
    val step = actionQueue.dequeue()
    val cellInc = step match {
        case AddX(v) => v
        case Noop() => 0
    }
    tick = tick + 1
    cellValue = cellValue + cellInc
    MachineState(tick, cellValue)
  }
}

case class MachineState(val tick: Int, val cell: Int)