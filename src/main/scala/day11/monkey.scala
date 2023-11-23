package day11

case class Monkey(
    id: Int,
    heldItem: Option[Long],
    items: List[Long],
    inspect: (Long) => Long,
    toss: (Long) => Int,
    isDangerous: Boolean = false
) {
  def print() = {
    println(s"Monkey ${id}: Items: (${items.map(_.toString).mkString(", ")}) ")
  }
  def short() = {
    s"${items.length}"
  }
}

object Monkey {
    def apply(base: Monkey, isDangerous: Boolean): Monkey = {
      Monkey(
        base.id,
        base.heldItem,
        base.items,
        base.inspect,
        base.toss,
        isDangerous
      )
    }
}

object MonkeyFun {
  def tossItem(m: Monkey): (Long, Monkey, Int) = {
    val holdFn           = if (m.isDangerous) holdItemDangerously(_) else holdItem(_)
    val thoughtfulMonkey = holdFn(m)
    val itemToThrow      = thoughtfulMonkey.heldItem.get
    val targetMonkeyId   = thoughtfulMonkey.toss(itemToThrow)
    // println(s" -> throw to $targetMonkeyId")
    val updatedMonkey = Monkey(
      thoughtfulMonkey.id,
      None,
      thoughtfulMonkey.items,
      thoughtfulMonkey.inspect,
      thoughtfulMonkey.toss
    )
    (itemToThrow, updatedMonkey, targetMonkeyId)
  }

  def giveItemToMonkey(m: Monkey, item: Long): Monkey = {
    Monkey(m.id, None, m.items :+ item, m.inspect, m.toss)
  }

  def holdItem(m: Monkey): Monkey = {
    val nextItem           = m.items.head
    val inspectedItemWorry = m.inspect(nextItem)
    val newItemValue       = inspectedItemWorry / 3
    // print(s"i $nextItem => $inspectedItemWorry => $newItemValue")

    Monkey(m.id, Some(newItemValue), m.items.tail, m.inspect, m.toss)
  }

  def holdItemDangerously(m: Monkey): Monkey = {
    val nextItem           = m.items.head
    val inspectedItemWorry = m.inspect(nextItem)

    Monkey(m.id, Some(inspectedItemWorry), m.items.tail, m.inspect, m.toss)
  }
}
