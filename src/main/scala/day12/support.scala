package day12

case class Coordinate(val row: Int, val col: Int) {
  def isSet() = {
    this != Coordinate(-1, -1)
  }
  def up(): Coordinate = {
    Coordinate(this.row - 1, this.col)
  }
  def down(): Coordinate = {
    Coordinate(this.row + 1, this.col)
  }
  def left(): Coordinate = {
    Coordinate(this.row, this.col - 1)
  }
  def right(): Coordinate = {
    Coordinate(this.row, this.col + 1)
  }

}

object Coordinate {
  def apply(): Coordinate = {
    Coordinate(-1, -1)
  }
}

case class CellScore(
    val coord: Coordinate,
    val cost: Int,
    val distance: Int
) extends Ordered[CellScore] {
  def f() = {
    cost + distance
  }
  override def compare(other: CellScore): Int = {
    this.f - other.f
  }

  override def toString(): String = {
    s"CS:${coord}: ${f}"
  }


}

case class Grid[T](val map: Array[Array[T]]) {
  def at(row: Int, col: Int) = {
    map(row)(col)
  }

  def at(coord: Coordinate) = {
    map(coord.row)(coord.col)
  }

  def render(renderChar: (T) => Char) = {
    map
      .map(line => {
        line.map(renderChar).mkString
      })
      .mkString("\n")
  }

  def render(renderChar: (T) => Char, overlay: Map[Coordinate, Char]) = {
    map.zipWithIndex
      .map(entry => {
        val row  = entry._2
        val line = entry._1
        line.zipWithIndex
          .map(lineEntry => {
            val col = lineEntry._2
            Coordinate(row, col) match {
              case coord if overlay.contains(coord) => overlay(coord)
              case coord                            => this.at(coord)
            }
          })
          .mkString
      })
      .mkString("\n")
  }

  def renderBetter(renderChar: (T) => String, overlay: Map[Coordinate, String]) = {
    map.zipWithIndex
      .map(entry => {
        val row  = entry._2
        val line = entry._1
        line.zipWithIndex
          .map(lineEntry => {
            val col = lineEntry._2
            Coordinate(row, col) match {
              case coord if overlay.contains(coord) => overlay(coord)
              case coord                            => this.at(coord)
            }
          })
          .mkString
      })
      .mkString("\n")
  }


  def intToStr(i: Int): String = {
    (i + 96).toChar.toString()
  }

  def intToChar(i: Int): Char = {
    (i + 96).toChar
  }

  def contains(c: Coordinate): Boolean = {
    c.row >= 0 &&
    c.row < map.length &&
    c.col >= 0 &&
    c.col < map(0).length
  }
}
