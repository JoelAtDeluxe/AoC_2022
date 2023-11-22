package day11

case class Monkey(
    id: Int,
    heldItem: Option[Int],
    items: List[Int],
    inspect: (Int) => Int,
    toss: (Int) => Int
) {
    def print() = {
        println(s"Monkey ${id}: Items: (${items.map(_.toString).mkString(", ")}) ")
    }
}

object MonkeyFun {
  def tossItem(m: Monkey): (Int, Monkey, Int) = {
    val thoughtfulMonkey = holdItem(m)
    val itemToThrow      = thoughtfulMonkey.heldItem.get
    val targetMonkeyId   = thoughtfulMonkey.toss(itemToThrow)

    val updatedMonkey = Monkey(
      thoughtfulMonkey.id,
      None,
      thoughtfulMonkey.items,
      thoughtfulMonkey.inspect,
      thoughtfulMonkey.toss
    )
    (itemToThrow, updatedMonkey, targetMonkeyId)
  }

  def giveItemToMonkey(m: Monkey, item: Int): Monkey = {
    Monkey(m.id, None, m.items :+ item, m.inspect, m.toss)
  }

  def holdItem(m: Monkey): Monkey = {
    val nextItem     = m.items.head
    val newItemValue = (m.inspect(nextItem)) / 3

    Monkey(m.id, Some(newItemValue), m.items.tail, m.inspect, m.toss)
  }
}