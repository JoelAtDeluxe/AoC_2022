package day08

package day08

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

class SetStore() extends Store[(Int, Int)] {
  var backing: Set[(Int, Int)] = new HashSet[(Int, Int)]()

  override def contains(key: (Int, Int)): Boolean = {
    this.backing.contains(key)
  }

  override def store(key: (Int, Int)): Boolean = {
    val newBacking = this.backing.+(key)
    val rtn = newBacking.size > backing.size
    backing = newBacking

    rtn
  }

  override def length: Int = {
    backing.size
  }

}
