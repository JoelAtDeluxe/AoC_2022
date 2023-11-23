package day11

case class Monkey(
    id: Int,
    heldItem: Option[Long],
    items: List[Long],
    inspect: (Long) => Long,
    toss: (Long) => Int,
    testDivisor: Long,
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
      testDivisor = base.testDivisor,
      isDangerous = isDangerous.getOrElse(base.isDangerous)
    )
  }
}

object MonkeyFun {
  def buildTossItemFn(
    commonDivisor: Option[Long]
  ): (Monkey) => (Long, Monkey, Int) = {
    val updateValFn = commonDivisor match {
        case Some(div) => (x: Long) => x % div
        case _ => (x: Long) => x
      }

    def tossItem(m: Monkey): (Long, Monkey, Int) = {
      val holdFn           = if (m.isDangerous) holdItemDangerously(_) else holdItem(_)
      val thoughtfulMonkey = holdFn(m)
      val itemToThrow      = thoughtfulMonkey.heldItem.get
      val updatedItemToThrow = updateValFn(itemToThrow)

      val targetMonkeyId   = thoughtfulMonkey.toss(updatedItemToThrow)
      
      val updatedMonkey = Monkey.update(thoughtfulMonkey, heldItem=Some(None))
      (updatedItemToThrow, updatedMonkey, targetMonkeyId)
    }
    tossItem
  }

  // Removed -- the numbers get to big with this impl
  // def tossItem(m: Monkey): (Long, Monkey, Int) = {
  //   val holdFn           = if (m.isDangerous) holdItemDangerously(_) else holdItem(_)
  //   val thoughtfulMonkey = holdFn(m)
  //   val itemToThrow      = thoughtfulMonkey.heldItem.get
  //   val targetMonkeyId   = thoughtfulMonkey.toss(itemToThrow)
  //   // println(s" -> throw to $targetMonkeyId")
  //   val updatedMonkey = Monkey.update(thoughtfulMonkey, heldItem=Some(None))
  //   (itemToThrow, updatedMonkey, targetMonkeyId)
  // }

  def giveItemToMonkey(m: Monkey, item: Long): Monkey = {
    Monkey.update(m, heldItem=Some(None), items=Some(m.items :+ item) )
  }

  def holdItem(m: Monkey): Monkey = {
    val nextItem           = m.items.head
    val inspectedItemWorry = m.inspect(nextItem)
    val newItemValue       = inspectedItemWorry / 3
    Monkey.update(m, heldItem=Some(Some(newItemValue)), items=Some(m.items.tail))
  }

  def holdItemDangerously(m: Monkey): Monkey = {
    val nextItem           = m.items.head
    val inspectedItemWorry = m.inspect(nextItem)
    Monkey.update(m, heldItem=Some(Some(inspectedItemWorry)), items=Some(m.items.tail))
  }
}
