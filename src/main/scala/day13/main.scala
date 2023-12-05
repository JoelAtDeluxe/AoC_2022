package day13

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import ujson.Value
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) = {
    val (left, right) = Source
      .fromResource("days11_20/day13_input.txt")
      .getLines()
      .grouped(3)
      .map(_.take(2))
      .foldLeft((List[String](), List[String]()))((acc, cur) => {
        (acc._1 :+ cur(0), acc._2 :+ cur(1))
      })

    val partOneResult = (0 until left.length)
      .map(index => {
        compareSignal(left(index), right(index), index + 1)
      })
      .sum

    println(s"Part 1: Summed signals are: $partOneResult")

    val extras = List(
        "[[2]]",
        "[[6]]",
    )

    val partTwoData = left ++ right ++ extras

    val sortedSignal = partTwoData.sortWith((s1, s2) => {
        val result = compareSignal(s1, s2, 1)
        result == 1 
    })
    
    val ex1Index = sortedSignal.indexOf(extras(0)) + 1
    val ex2Index = sortedSignal.indexOf(extras(1)) + 1

    println(s"Part 2: sorted product: ${ex1Index * ex2Index}")


    println("Done")
  }

  def compareLists(
      left: ArrayBuffer[Value],
      right: ArrayBuffer[Value],
      show: Boolean
  ): CompareResult = {
    if (show) println(s"${left}  <  ${right}?")
    compareListsAt(left, right, 0, show)
  }

  @tailrec
  def compareListsAt(
      left: ArrayBuffer[Value],
      right: ArrayBuffer[Value],
      index: Int,
      show: Boolean
  ): CompareResult = {

    (index >= left.length, index >= right.length) match {
      case (true, true)  => Equal()
      case (true, false) => Smaller()
      case (false, true) => Greater()
      case _ => {
        val lval = left(index)
        val rval = right(index)

        val compareResult = (asInt(lval), asInt(rval)) match {
          case (None, None)    => compareLists(lval.arr, rval.arr, show)
          case (Some(l), None) => compareLists(ArrayBuffer(ujson.Num(l)), rval.arr, show)
          case (None, Some(r)) => compareLists(lval.arr, ArrayBuffer(ujson.Num(r)), show)
          case (Some(l), Some(r)) if l == r => Equal()
          case (Some(l), Some(r)) => if (l < r) Smaller() else Greater()
        }

        if (compareResult == Equal()) {
          compareListsAt(left, right, index + 1, show)
        } else compareResult

      }
    }
  }
  def asInt(v: Value) = { v.numOpt.map(_.toInt) }

  def compareSignal(
      leftVal: String,
      rightVal: String,
      puzIndex: Int,
      show: Boolean = false
  ): Int = {
    val lv = ujson.read(leftVal).arr
    val rv = ujson.read(rightVal).arr

    if (show) println(s"\n###### Index $puzIndex: ######## ")

    if (compareLists(lv, rv, show) == Smaller()) puzIndex else 0
  }

}

sealed trait CompareResult

case class Smaller() extends CompareResult
case class Greater() extends CompareResult
case class Equal()   extends CompareResult
