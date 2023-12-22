package day15

case class Range(from: Int, to: Int)

object Range {
  def merge(r1: Range, r2: Range): (Range, Option[Range]) = {
    // situations:
    // (  r1  ) (  r2  ) distinct (gap between)
    // ( r1 () r2 ) intersecting ( negative gap)
    // (r1)(r2) adjacent (no gap)
    // (r1 (r2) ) contained
    val (or1, or2) = if (r1.from > r2.from) (r2, r1) else (r1, r2)
    val lrGap      = or2.from - or1.to // (2->5) (4->6) 4-5 => -1, 0 => overlap, 1 => touch >1=>gap

    if (lrGap <= 1) { // touch or intersect, so return outer points
      (Range(or1.from, math.max(or1.to, or2.to)), None)
    } else {
      (r1, Some(r2))
    }
  }

  def flatten(r1: Range, rList: List[Range]): List[Range] = {
    var newRanges = List[Range]()
    var addRange  = r1
    for (foundRange <- rList) {
      val result = Range.merge(addRange, foundRange)
      addRange = result._1
      result._2.foreach(r => newRanges = r +: newRanges)
    }
    addRange +: newRanges
  }

  def bound(r: Range, limit: Range): Range = {
    Range(math.max(r.from, limit.from), math.min(r.to, limit.to))
  }
}
