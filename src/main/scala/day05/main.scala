package day05

import scala.io.Source
import scala.util.Try
import scala.collection.mutable.ListBuffer

object Main {

  def main(args: Array[String]) {
    val lines = Source
      .fromResource("day05_input.txt")
      .getLines()
      .toList

    val break      = lines.indexWhere(_.trim.isEmpty())
    val diagramRaw = lines.slice(0, break)
    val queues     = parseDiagram(diagramRaw)

    val instructionsRaw = lines.slice(break + 1, lines.length)
    val instructions = instructionsRaw
      // .map(l => ("""move (\d+) from (\d+) to (\d+)"""))
      .map(_.split(" +"))
      .map(l => (l(1).toInt, l(3), l(5)))
      .map(p => Instruction(p._1, p._2, p._3))

    var queue9000 = queues.toSeq.toMap
    instructions.foreach(inst => {
      val floatingElems = singleItemMove(queue9000(inst.source), inst.quantity)
      queue9000 = queue9000 ++ Map(
        inst.destination -> (queue9000(inst.destination) ++ floatingElems),
        inst.source -> queue9000(inst.source).dropRight(inst.quantity)        
      )
    })
    val lifer9000Tops = queue9000.toSeq
      .sortBy(entry => entry._1)
      .map( entry => entry._2.last)
      .mkString
    println(s"Part 1: Top boxes in order: $lifer9000Tops")

    var queue9001 = queues.toSeq.toMap

    instructions.foreach(inst => {
      val floatingElems = queue9001(inst.source).takeRight(inst.quantity)
      queue9001 = queue9001 ++ Map(
        inst.destination -> (queue9001(inst.destination) ++ floatingElems),
        inst.source -> queue9001(inst.source).dropRight(inst.quantity)        
      )
    })

    val lifer9001Tops = queue9001.toSeq
      .sortBy(entry => entry._1)
      .map( entry => entry._2.last)
      .mkString
    println(s"Part 2: Top boxes in order: $lifer9001Tops")
  }

  def parseDiagram(diagram: List[String]) : Map[String, List[String]] = {
    val rDiagram = diagram.reverse
    val labelRow = rDiagram.head
      .grouped(4)
      .map(_.trim)
      .toList
    val queues = labelRow
      .map(v => (v, ListBuffer[String]()))
      .toMap

    rDiagram.tail // working on this kind of row: [Z] [M] [P]
      .map(
        _.grouped(4)
          .map(_.trim)
          .map(s => if (s.isEmpty) " " else s) // restore a space if empty to keep a gap
          .map(_.stripPrefix("[").stripSuffix("]"))
          .mkString
      )
      .foreach( // entries look like: "ZMP" (or "Z P" if thre's a gap)
        _.zipWithIndex.foreach({ case (c, idx) =>
          if (c != ' ') {
            val label = labelRow(idx)
            queues(label).append(c.toString())
          }
        })
      )

    queues.map(entry => (entry._1, entry._2.toList))
  }

  def singleItemMove(lst: List[String], times: Int): List[String] = {
    (0 until times)
      .map(idx => {
        lst.takeRight(1+idx)(0)
      })
      .toList
  }

  def printQueues(queues: Map[String, List[String]]) = {
    queues.toSeq.sortBy(entry => entry._1).map( entry => {
      s"${entry._1}: ${entry._2.mkString(" ")}"
    }).foreach(println)
  }

}

case class Instruction(
  quantity: Int,
  source: String,
  destination: String
  ) {
  def print() = {
    println(s"From ${source} Move ${quantity} to ${destination}")
  }
}
