package day09

sealed trait Direction

case class Up() extends Direction
case class Down() extends Direction
case class Left() extends Direction
case class Right() extends Direction