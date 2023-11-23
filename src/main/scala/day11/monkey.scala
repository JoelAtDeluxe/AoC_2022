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
  def update(
      base: Monkey,
      heldItem: Option[Option[Long]] = None,
      items: Option[List[Long]] = None,
      isDangerous: Option[Boolean] = None
  ): Monkey = {
    Monkey(
      id = base.id,
      heldItem = heldItem.getOrElse(base.heldItem),
      items = items.getOrElse(base.items),
      inspect = base.inspect,
      toss = base.toss,
      isDangerous = isDangerous.getOrElse(base.isDangerous)
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
    val updatedMonkey = Monkey.update(thoughtfulMonkey, heldItem=Some(None))
    (itemToThrow, updatedMonkey, targetMonkeyId)
  }

  def giveItemToMonkey(m: Monkey, item: Long): Monkey = {
    Monkey.update(m, heldItem=Some(None), items=Some(m.items :+ item) )
  }

  def holdItem(m: Monkey): Monkey = {
    val nextItem           = m.items.head
    val inspectedItemWorry = m.inspect(nextItem)
    val newItemValue       = inspectedItemWorry / 3
    // print(s"BAD! i $nextItem => $inspectedItemWorry => $newItemValue")
    Monkey.update(m, heldItem=Some(Some(newItemValue)), items=Some(m.items.tail))
  }

  def holdItemDangerously(m: Monkey): Monkey = {
    val nextItem           = m.items.head
    val inspectedItemWorry = m.inspect(nextItem)
    // print(s"i $nextItem => $inspectedItemWorry")
    Monkey.update(m, heldItem=Some(Some(inspectedItemWorry)), items=Some(m.items.tail))
  }
}
