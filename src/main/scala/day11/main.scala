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

    var inspectedCounts = monkeys.map(m => 0l)

    def callback(monkeyId: Int, numTossed: Long) = {
      inspectedCounts(monkeyId) += numTossed
      // println(s"=> ${inspectedCounts.mkString(", ")}")
    }

    // println("Initial State: ")
    // println(s"*  ${monkeys.map(_.short()).mkString(", ")}")
    val numRounds = 20
    runRounds(monkeys, numRounds, callback)

    val topTwo = inspectedCounts.sorted.reverse.slice(0, 2)

    println(s"Part 1: At the end of $numRounds rounds, the top two monkeys handled ${topTwo.mkString(", and ")} items")
    println(s"The level of monkey business is: ${topTwo.product}")

    inspectedCounts = monkeys.map(m => 0l)
    
    val dangerousMonkeys = monkeys.map(m => Monkey(m, true))
    runRounds(dangerousMonkeys, 20, callback)
    println(s"=> ${inspectedCounts.mkString(", ")}")

    val topTwoDangerous = inspectedCounts.sorted.reverse.slice(0, 2)

    println(s"Part 2: At the end of 10000 rounds, the top two dangerous monkeys handled ${topTwoDangerous.mkString(", and ")} items")
    println(s"The level of monkey business is: ${topTwoDangerous.product}")
  }

  @tailrec
  def runRounds(
      monkeys: Array[Monkey],
      roundsRemaining: Int,
      onTossed: (Int, Long) => Unit
  ): Array[Monkey] = {
    if (roundsRemaining <= 0) {
      monkeys
    } else {
      val newMonkeys = runRound(monkeys, 0, onTossed)
      runRounds(newMonkeys, roundsRemaining - 1, onTossed)
    }
  }

  @tailrec
  def runRound(
      monkeys: Array[Monkey],
      startFrom: Int = 0,
      onTossed: (Int, Long) => Unit
  ): Array[Monkey] = {
    if (startFrom < monkeys.length) {
      val tosser     = monkeys(startFrom)
      val newMonkeys = runTurn(monkeys, startFrom)

      // println(s"Turn ${startFrom} End!\n!  ${newMonkeys.map(_.short()).mkString(", ")}")

      onTossed(startFrom, tosser.items.length)
      runRound(newMonkeys, startFrom + 1, onTossed)
    } else {
      // println("End of round!")
      // println("==========================")
      // monkeys.foreach(_.print())
      // println(">>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<")
      monkeys
    }
  }

  @tailrec
  def runTurn(monkeys: Array[Monkey], monkeyIndex: Int): Array[Monkey] = {
    if (monkeys(monkeyIndex).items.isEmpty) {
      monkeys
    } else {
      val currentMonkey                     = monkeys(monkeyIndex)
      val (item, newMonkey, targetMonkeyId) = MonkeyFun.tossItem(currentMonkey)

      val newMonkeyState = monkeys.map(m => {
        m.id match {
          case `currentMonkey`.id => newMonkey
          case `targetMonkeyId`   => MonkeyFun.giveItemToMonkey(m, item)
          case _                  => m
        }
      })
      runTurn(newMonkeyState, monkeyIndex)
    }
  }

  def parseMonkey(monkeyBlock: String) = {
    // let's not worry too much about genericness here, and instead focus on the structure
    val lines = monkeyBlock.split("\n")
    val id    = lines(0).split(" ", 2)(1).stripSuffix(":").trim.toInt
    val items = lines(1).split(":", 2)(1).split(",").map(_.trim.toLong).toList

    val opLogicTokens = lines(2).split("=", 2)(1).trim.split(" ")
    val inspect       = BinaryOperation.build(opLogicTokens(1), opLogicTokens(0), opLogicTokens(2))

    val testDivisor = lines(3).split("divisible by", 2)(1).trim.toLong
    val ifTrue      = lines(4).split("throw to monkey")(1).trim.toInt
    val ifFalse     = lines(5).split("throw to monkey")(1).trim.toInt
    val toss = (x: Long) => {
      if (x % testDivisor == 0) {
        ifTrue
      } else {
        ifFalse
      }
    }
    Monkey(id, None, items, inspect, toss)
  }

}
