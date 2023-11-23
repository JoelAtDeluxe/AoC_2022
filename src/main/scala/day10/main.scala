package day10

import scala.io.Source


object Main {
  def main(args: Array[String]) = {
    val cmdFeed = Source.fromResource("days01_10/day10_input.txt").getLines()

    val partFeeds = cmdFeed.duplicate
    
    var signalSum = new Ticker(partFeeds._1)
      .zipWithIndex
      .map(entry => (entry._1, entry._2+1)) // fix tick vs index (1 based vs 0 based)
      .filter(entry => entry._2 == 20 || ((entry._2 - 20) % 40 == 0))
      .map(entry => entry._2 * entry._1.nextValue)
      .sum
    println(s"Part 1: Sum of signal samples: $signalSum\n")

    println("Part 2: Look for the capital letters please!")
    new Ticker(partFeeds._2)
      .zipWithIndex
      .map(entry => (entry._1, entry._2+1)) // fix tick vs index (1 based vs 0 based)
      .foreach( entry => {
        val spriteMiddlePos = entry._1.lastValue
        val crtCell = (entry._2 - 1) % 40

        if (crtCell == 0) {
          println()
        }
        val ch = if (within(crtCell, spriteMiddlePos-1, spriteMiddlePos+1)) "#" else " "
        print(ch)
      })
    println()

  }

  def within(v: Int, lower: Int, upper: Int): Boolean = {
    v >= lower && v <= upper
  }

  // debugging tool
  def genLine(spriteMid: Int, tickPos: Int): String = {
    val activeCell = (tickPos-1) % 40
    val emptyLine = "." * 40
    val sprite = "###".zipWithIndex.map(entry => {
      if ((spriteMid + entry._2 - 1) == activeCell) "!" else "#"
    }).mkString
    emptyLine.slice(0, spriteMid-1) + sprite + emptyLine.slice(spriteMid+2, 40)
  }
}
