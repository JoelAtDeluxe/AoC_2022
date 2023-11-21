package day10

sealed trait Action {
    def ticks(): Int
}

case class AddX(val num:Int) extends Action {
    override def ticks(): Int = 2
}

case class Noop() extends Action {
    override def ticks(): Int = 1
}