package day11

import scala.io.Source
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) = {
    val monkeys = Source
      .fromResource("day11_input.txt")
      .mkString
      .split("\n\n")
      .map(monkey => parseMonkey(monkey))

    val (_, inspectedCounts) = runRounds(monkeys, 20, monkeys.map(m => 0l))
    val topTwo = inspectedCounts.sorted.reverse.slice(0, 2)

    println(s"Part 1: At the end of 20 rounds, the top two monkeys handled ${topTwo.mkString(", and ")} items")
    println(s"The level of monkey business is: ${topTwo.product}")
    
    // val dangerousMonkeys = monkeys.map(m => Monkey.update(m, isDangerous=Some(true)))
    // val (_, dangerousInspectCounts) = runRounds(dangerousMonkeys, 20, dangerousMonkeys.map(m => 0l))
    // println(s"=> ${dangerousInspectCounts.mkString(", ")}")

    // val topTwoDangerous = dangerousInspectCounts.sorted.reverse.slice(0, 2)

    // println(s"Part 2: At the end of 10000 rounds, the top two dangerous monkeys handled ${topTwoDangerous.mkString(", and ")} items")
    // println(s"The level of monkey business is: ${topTwoDangerous.product}")
  }

  @tailrec
  def runRounds(
      monkeys: Array[Monkey],
      roundsRemaining: Int,
      tossedCounts: Array[Long]
  ): (Array[Monkey], Array[Long]) = {
    if (roundsRemaining <= 0) {
      (monkeys, tossedCounts)
    } else {
      val (newMonkeys, updatedTossCounts) = runRound(monkeys, 0, tossedCounts)
      runRounds(newMonkeys, roundsRemaining - 1, updatedTossCounts)
    }
  }

  @tailrec
  def runRound(
      monkeys: Array[Monkey],
      startFrom: Int = 0,
      tossedCounts: Array[Long]
  ): (Array[Monkey], Array[Long]) = {
    if (startFrom < monkeys.length) {
      val tosser     = monkeys(startFrom)
      val newMonkeys = runTurn(monkeys, startFrom)

      // println(s"Turn ${startFrom} End!\n!  ${newMonkeys.map(_.short()).mkString(", ")}")

      var updatedTossCount = addTossToCount(tossedCounts, startFrom, tosser.items.length)
      runRound(newMonkeys, startFrom + 1, updatedTossCount)
    } else {
      // println("End of round!")
      // println("==========================")
      // monkeys.foreach(_.print())
      // println(">>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<")
      (monkeys, tossedCounts)
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

  def addTossToCount(tossCnt: Array[Long], updatedIndex: Int, qty: Long): Array[Long] = {
    tossCnt.zipWithIndex.map(entry => if (entry._2 == updatedIndex) qty + entry._1 else entry._1 )
  }

}
