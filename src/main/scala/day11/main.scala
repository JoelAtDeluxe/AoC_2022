package day11

import scala.io.Source
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) = {
    val monkeys = Source
      .fromResource("day11_sample.txt")
      .mkString
      .split("\n\n")
      .map(monkey => parseMonkey(monkey))

    // rounds
    // (0 until 20).foreach{ // round
    //   someList.foreach(m => {
    //     var currentMonkey = m

    //     if (currentMonkey.items.nonEmpty) {
    //       val result = MonkeyFun.tossItem(m)
    //       val tossedItem = result._1
    //       val currentMonkey = result._2
    //       val tagetMonkeyId = result._3
    //     }

    //   })
    // }
    val inspectedCounts = monkeys.map(m => 0)
    val newMonkeyState = runRound(monkeys, 0, (monkeyId, numTossed)=>{ inspectedCounts(monkeyId) += numTossed })
    
    newMonkeyState.foreach(_.print)
    println(inspectedCounts.map(_.toString).mkString(", "))

    // var myMonkey = monkeys(2)
    // while (myMonkey.items.nonEmpty) {
    //   val result = MonkeyFun.tossItem(myMonkey)
    //   println(
    //     s"My monkey tosses item (${myMonkey.items.head}), now worth ${result._1} to monkey ${result._3}"
    //   )
    //   myMonkey = result._2 // eep!
    // }

    println()
  }

  @tailrec
  def runRound(monkeys: Array[Monkey], startFrom: Int = 0, onTossed: (Int, Int)=>Unit): Array[Monkey] = {
    if (startFrom < monkeys.length) {
      val tosser = monkeys(startFrom)
      val newMonkeys = runTurn(monkeys, startFrom)
      onTossed(startFrom, tosser.items.length)
      runRound(newMonkeys, startFrom + 1, onTossed)
    }
    else {
      monkeys
    }
  }

  @tailrec
  def runTurn(monkeys: Array[Monkey], monkeyIndex: Int): Array[Monkey] = {
    if (monkeys(monkeyIndex).items.isEmpty) {
      monkeys
    }
    else {
      val currentMonkey = monkeys(monkeyIndex)
      val (item, newMonkey, targetMonkeyId) = MonkeyFun.tossItem(currentMonkey)

      // println(
      //   s"My monkey tosses item (${currentMonkey.items.head}), now worth ${item} to monkey ${targetMonkeyId}"
      // )

      val newMonkeyState = monkeys.map( m => {
        m.id match {
          case `currentMonkey`.id => newMonkey
          case `targetMonkeyId` => MonkeyFun.giveItemToMonkey(m, item)
          case _ => m
        }
      })
      runTurn(newMonkeyState, monkeyIndex)
    }
  }

  def parseMonkey(monkeyBlock: String) = {
    // let's not worry too much about genericness here, and instead focus on the structure
    val lines = monkeyBlock.split("\n")
    val id    = lines(0).split(" ", 2)(1).stripSuffix(":").trim.toInt
    val items = lines(1).split(":", 2)(1).split(",").map(_.trim.toInt).toList

    val opLogicTokens = lines(2).split("=", 2)(1).trim.split(" ")
    val inspect       = BinaryOperation.build(opLogicTokens(1), opLogicTokens(0), opLogicTokens(2))

    val testDivisor = lines(3).split("divisible by", 2)(1).trim.toInt
    val ifTrue      = lines(4).split("throw to monkey")(1).trim.toInt
    val ifFalse     = lines(5).split("throw to monkey")(1).trim.toInt
    val toss = (x: Int) => {
      if (x % testDivisor == 0) {
        ifTrue
      } else {
        ifFalse
      }
    }
    Monkey(id, None, items, inspect, toss)
  }

}
