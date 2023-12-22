package day15

import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable
import java.util.ArrayList
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) = {

    val rawSensorBeacons = Source
      .fromResource("days11_20/day15_input.txt")
      .getLines()
      .map(line =>
        """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""".r
          .findFirstMatchIn(line)
          .get
          .subgroups
          .map(_.toInt)
      )
      .map(line => {
        // given in (x, y), but x actually maps closer to column than row
        val beacon = Beacon(line(3), line(2))
        val sensor = Sensor(line(1), line(0), beacon)
        (sensor, beacon)
      })
      .toList
    val sensors = rawSensorBeacons.map(v => v._1)
    val beacons = rawSensorBeacons.map(v => v._2).toSet.toList

    val targetRow = 2000000
    val part1     = countBeaconFreeSpacesOnRow(sensors, beacons, targetRow)
    println(s"Beacon-free spaces in row $targetRow => $part1")

    val gaps = searchForGap(sensors, beacons, 0, 4000000)
    // println(gaps)
    if (gaps.length == 1) {
      val num = (gaps(0).column.longValue * 4000000) + gaps(0).row.longValue()
      println(s"Part 2: Tunning Freq for this beacon is: $num")
    } else {
      println("Scan results inconclusive")
    }
  }

  @tailrec
  def searchForGap(sensors: List[Sensor], beacons: List[Beacon], row: Int, stop: Int): List[Point] = {
    if (row >= stop) {
      List()
    } else {
      val gaps = findGap(sensors, beacons, row)
      if (gaps.length > 0) {
        gaps
      } else {
        searchForGap(sensors, beacons, row+1, stop)
      }
    }
  } 

  def countBeaconFreeSpacesOnRow(sensors: List[Sensor], beacons: List[Beacon], row: Int): Int = {
    var coveredRanges = List[Range]()

    sensors.foreach(sen => {
      val sensorRadius = distance(sen.toPoint, sen.beacon.toPoint)
      val gap          = math.abs(sen.row - row)
      val overlap      = (sensorRadius - gap)
      if (overlap >= 0) {
        val sensorRange = Range(sen.column - overlap, sen.column + overlap)
        coveredRanges = Range.flatten(sensorRange, coveredRanges)
      }
    })
    val covered = coveredRanges.map(r => r.to - r.from + 1).sum
    covered - beacons.count(b => b.row == row)
  }

  def findGap(sensors: List[Sensor], beacons: List[Beacon], row: Int): List[Point] = {
    var coveredRanges = List[Range]()

    sensors.foreach(sen => {
      val sensorRadius = distance(sen.toPoint, sen.beacon.toPoint)
      val gap          = math.abs(sen.row - row)
      val overlap      = (sensorRadius - gap)
      if (overlap >= 0) {
        val sensorRange = Range(sen.column - overlap, sen.column + overlap)
        coveredRanges = Range.flatten(sensorRange, coveredRanges)
      }
    })
    val beaconsInRow = beacons.filter(b => b.row == row)
    val fixedRanges = coveredRanges
      .map(r => Range.bound(r, Range(0, 4000000) ))

    var gapRanges = List[scala.collection.immutable.Range]()
    var prev = 0
    for (r <- fixedRanges) {
      gapRanges = (prev until r.from) +: gapRanges
      prev = r.to+1
    }
    gapRanges = (prev until 4000000) +: gapRanges
    val gaps = gapRanges.map(_.toArray).flatten
    val trueGaps = gaps.diff(beaconsInRow.map(b => b.column))

    trueGaps.map( g => Point(row, g))
  }

  def distance(p1: Point, p2: Point) = {
    math.abs(p1.row - p2.row) + math.abs(p1.column - p2.column)
  }
}


case class Point(row: Int, column: Int)

case class Beacon(row: Int, column: Int) {
  def toPoint(): Point = {
    Point(row, column)
  }
}

case class Sensor(row: Int, column: Int, beacon: Beacon) {
  def toPoint(): Point = {
    Point(row, column)
  }
}
