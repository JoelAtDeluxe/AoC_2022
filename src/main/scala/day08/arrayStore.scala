package day08

import scala.annotation.tailrec

class ArrayStore(val initialCapacity: Int = 128) extends Store[(Int, Int)] {
  var backing: Array[(Int, Int)] = new Array(initialCapacity)
  var index             = 0

  override def length(): Int = {
    index
  }

  override def contains(key: (Int, Int)): Boolean = {
    containsFrom(key, 0)
  }

  @tailrec
  final def containsFrom(key: (Int, Int), index: Int): Boolean = {
    index match {
      case _ if index >= backing.length => false
      case _ if backing(index) == key   => true
      case i                            => containsFrom(key, i + 1)
    }
  }

  override def store(key: (Int, Int)): Boolean = {
    if (!this.contains(key)) {
      if (needToGrow()) {
        resize()
      }
      backing(index) = key
      index = index + 1
        true
    } else false
  }

  def needToGrow(): Boolean = {
    index == backing.length
  }

  def resize() = {
    val newCapacity          = backing.length * 2
    val newBacking: Array[(Int, Int)] = new Array(newCapacity)
    backing.zipWithIndex.foreach(entry => {
      newBacking(entry._2) = entry._1
    })
    backing = newBacking
  }
}
