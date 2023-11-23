package day08

import scala.io.Source
import scala.annotation.tailrec
import _root_.day08.day08.SetStore

object Main {
  def main(args: Array[String]) = {
    val zero = '0'.toInt // 48
    val grid = Source
      .fromResource("days01_10/day08_input.txt")
      .getLines()
      .map(line => line.chars().map(c => c.toInt - zero).toArray())
      .toArray

    val start = System.currentTimeMillis()
    (0 until 1).foreach(_ => {
      val result = countVisibleTrees(grid, new SetStore())

      // brute force -- is there a better way?
      val highestScore = (0 until grid.length)
        .map(rowIndex => 
          (0 until grid(0).length).map(c => (rowIndex, c)) 
        )
        .flatten
        .foldLeft(0)( (acc, cur) => {
          val entry = treeScenicScore(grid, cur._1, cur._2)
          if (entry > acc) {
            entry
          } else acc
        } )
    })
    println(s"Done! Time: ${System.currentTimeMillis() - start}")
  }

  def treeScenicScore(grid: Array[Array[Int]], row: Int, col: Int): Int = {
    val treehouseHeight = getTree(grid, (row, col))
    if (treehouseHeight == 0)
      1
    else {
      val upTreeScore = visibleTrees(grid, (row-1, col), grow=(-1, 0), treehouseHeight)
      val downTreeScore = visibleTrees(grid, (row+1, col), grow=(1, 0), treehouseHeight)
      val leftTreeScore = visibleTrees(grid, (row, col-1), grow=(0, -1), treehouseHeight)
      val rightTreeScore = visibleTrees(grid, (row, col+1), grow=(0, 1), treehouseHeight)
      upTreeScore * downTreeScore * leftTreeScore * rightTreeScore
    }
  }

  def visibleTrees(
    grid: Array[Array[Int]],
    coord: (Int, Int),
    grow: (Int, Int),
    maxHeight: Int,
    count: Int = 0
  ): Int = {
    if (!within(grid, coord))
      count
    else {
      val thisTree = getTree(grid, coord)
      thisTree match {
        case tooTall if tooTall >= maxHeight => count + 1
        case smaller =>
          visibleTrees(grid, (coord._1 + grow._1, coord._2 + grow._2), grow, maxHeight, count + 1)
      }
    } 
  }

  def countVisibleTrees(
      grid: Array[Array[Int]],
      visibleTrees: Store[(Int, Int)]
  ) = {
    // left-to-right
    (0 until grid.length)
      .map(i => treesInRow(grid, i, 0, 1, List()))
      .flatten
      .map(treeCoord => visibleTrees.store(treeCoord))

    // right-to-left
    (0 until grid.length)
      .map(i => treesInRow(grid, i, grid(0).length - 1, -1, List()))
      .flatten
      .map(treeCoord => visibleTrees.store(treeCoord))

    (0 until grid(0).length)
      .map(i => treesInCol(grid, 0, i, 1, List()))
      .flatten
      .map(treeCoord => visibleTrees.store(treeCoord))

    (0 until grid(0).length)
      .map(i => treesInCol(grid, grid.length - 1, i, -1, List()))
      .flatten
      .map(treeCoord => visibleTrees.store(treeCoord))

    visibleTrees.length
  }

  @tailrec
  def treesInRow(
      grid: Array[Array[Int]],
      row: Int,
      col: Int,
      grow: Int,
      tallTrees: List[(Int, Int)]
  ): List[(Int, Int)] = {
    val nextCol = col + grow

    if (!within(col, 0, grid(0).length)) {
      tallTrees
    } else if (tallTrees.length == 0) {
      treesInRow(grid, row, nextCol, grow, (row, col) +: tallTrees)
    } else if (getTree(grid, tallTrees.head) == 9) {
      tallTrees
    }
    else {
      grid(row)(col) match {
        case v if v > getTree(grid, tallTrees.head) =>
          treesInRow(grid, row, nextCol, grow, (row, col) +: tallTrees)
        case _ => treesInRow(grid, row, nextCol, grow, tallTrees)
      }
    }
  }

  @tailrec
  def treesInCol(
      grid: Array[Array[Int]],
      row: Int,
      col: Int,
      grow: Int,
      tallTrees: List[(Int, Int)]
  ): List[(Int, Int)] = {
    val nextRow = row + grow

    if (!within(row, 0, grid.length)) {
      tallTrees
    } else if (tallTrees.length == 0) {
      treesInCol(grid, nextRow, col, grow, (row, col) +: tallTrees)
    } else if (getTree(grid, tallTrees.head) == 9) {
      tallTrees
    } 
    else
      grid(row)(col) match {
        case v if v > getTree(grid, tallTrees.head) =>
          treesInCol(grid, nextRow, col, grow, (row, col) +: tallTrees)
        case _ => treesInCol(grid, nextRow, col, grow, tallTrees)
      }
  }

  def within(v: Int, lower: Int, upper: Int): Boolean = {
    v >= lower && v < upper
  }
  def within(g: Array[Array[Int]], coord: (Int, Int)): Boolean = {
    within(coord._1, 0, g.length) && within(coord._2, 0, g(0).length)
  }

  def getTree(grid: Array[Array[Int]], coord: (Int, Int)): Int = {
    grid(coord._1)(coord._2)
  }

}
