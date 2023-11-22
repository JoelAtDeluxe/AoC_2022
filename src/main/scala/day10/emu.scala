package day10

import scala.collection.mutable.Queue

class Ticker(val source: Iterator[String]) extends Iterator[MachineState] {
  var cellValue = 1 // TODO: see if you can drop this state 
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
    val lastValue = cellValue
    cellValue = cellValue + cellInc
    MachineState(lastValue, cellValue)
  }
}

case class MachineState(lastValue: Int, nextValue: Int)