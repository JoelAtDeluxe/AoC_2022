package day12

import scala.collection.mutable.ArrayBuffer
import Ordering.Implicits._

object HeapMain {
  def main(args: Array[String]) = {
    println("Yo!")
    
    // testOne()
    val h = new MinHeap[Int]()
    assert(h.render() == "[]")

    h.addNode(50)
    h.addNode(60)
    h.addNode(30)
    assert(h.render() == "[30, 60, 50]")

    h.addNode(75)
    assert(h.render() == "[30, 60, 50, 75]")

    h.addNode(51)
    assert(h.render() == "[30, 51, 50, 75, 60]")

    h.addNode(49)
    assert(h.render() == "[30, 51, 49, 75, 60, 50]")

    h.addNode(25)
    assert(h.render() == "[25, 51, 30, 75, 60, 50, 49]")

    var index = h.find((i) => i == 30)
    assert( index == 2)
    h.update(10, index)
    assert(h.render() == "[10, 51, 25, 75, 60, 50, 49]")
    h.update(30, 0)
    assert(h.render() == "[25, 51, 30, 75, 60, 50, 49]")

    h.update(100, index)
    assert(h.render() == "[25, 51, 49, 75, 60, 50, 100]")
    h.update(30, h.backing.length-1)
    assert(h.render() == "[25, 51, 30, 75, 60, 50, 49]")

    h.update(100, 0)
    assert(h.render() == "[30, 51, 49, 75, 60, 50, 100]")
    h.update(25, h.backing.length-1)
    assert(h.render() == "[25, 51, 30, 75, 60, 50, 49]")

    println("Bye")
  }

  def testOne() {
    val h = new MinHeap[Int]()
    assert(h.render() == "[]")

    h.addNode(50)
    assert(h.render() == "[50]")

    h.addNode(60)
    assert(h.render() == "[50, 60]")

    h.addNode(30)
    assert(h.render() == "[30, 60, 50]")

    h.addNode(75)
    assert(h.render() == "[30, 60, 50, 75]")

    h.addNode(51)
    assert(h.render() == "[30, 51, 50, 75, 60]")

    h.addNode(49)
    assert(h.render() == "[30, 51, 49, 75, 60, 50]")

    h.addNode(25)
    assert(h.render() == "[25, 51, 30, 75, 60, 50, 49]")

    assert(h.delete() == 25)
    assert(h.render() == "[30, 51, 49, 75, 60, 50]")

    assert(h.delete() == 30)
    assert(h.render() == "[49, 51, 50, 75, 60]")

    assert(h.delete() == 49)
    assert(h.render() == "[50, 51, 60, 75]")

    assert(h.delete() == 50)
    assert(h.render() == "[51, 75, 60]")

    assert(h.delete() == 51)
    assert(h.render() == "[60, 75]")

    assert(h.delete() == 60)
    assert(h.render() == "[75]")

    assert(h.delete() == 75)
    assert(h.render() == "[]")
  }
}

class MinHeap[T: Ordering] {
  val backing = ArrayBuffer[T]()

  def peak(): T = {
    return backing(0)
  }

  def addNode(item: T) = {
    val startLocation = backing.length
    backing.append(item)
    moveUp(startLocation, parentIndex(startLocation))
  }

  def moveUp(childIdx: Int, parentIdx: Int): Unit = {
    if (backing(childIdx) < backing(parentIdx)) {
      swapElems(childIdx, parentIdx)
      moveUp(parentIdx, parentIndex(parentIdx))
    }
  }

  def delete(): T = {
    swapElems(0, backing.length - 1)
    val result = backing.remove(backing.length - 1)
    if (backing.length > 0) {
      moveDown(0)
    }
    result
  }

  def moveDown(index: Int): Unit = {
    val (left, right) = childIndicies(index)
    val parentVal = backing(index)

    val lesserChild = (left, right) match {
      case (Some(l), Some(r)) => if (backing(l) < backing(r)) l else r
      case (Some(l), None) => l
      case (None, Some(r)) => r
      case (None, None) => -1
    }
    if (lesserChild > -1 && backing(lesserChild) < parentVal) {
      swapElems(index, lesserChild)
      moveDown(lesserChild)
    }
  }

  def find( fn: (T) => Boolean): Int = {
    backing.indexWhere(fn)
  }

  def update(newVal: T, index: Int) = {
    val oldValue = backing(index)
    backing(index) = newVal
    if (oldValue > newVal) {
      moveUp(index, parentIndex(index))
    } else {
      moveDown(index)
    }
  }

  // def removeNode(idx: Int): Unit = {
  //   // shouldn't need this for these problems
  //   swapElems(idx, backing.length - 1)
  //   backing.remove(backing.length - 1)
  // }

  // def moveDown(sourceIdx: Int): Unit = {
  //   // for deletes
  //   val sourceElemVal = backing(sourceIdx)

  //   val (left, right) = childIndicies(sourceIdx)

  //   val childIdx = (left, right) match {
  //       case (Some(l), Some(r)) => if(backing(l) < backing(r)) Some(l) else Some(r)
  //       case (Some(l), None) => Some(l)
  //       case (None, Some(r)) => Some(r)
  //       case _ => None
  //   }

  //   if (childIdx.isDefined && sourceElemVal > backing(childIdx.get)) {
  //       swapElems(sourceIdx, childIdx.get)
  //       moveDown(childIdx.get)
  //   }
  // }

  def swapElems(aIdx: Int, bIdx: Int) = {
    val temp = backing(aIdx)
    backing(aIdx) = backing(bIdx)
    backing(bIdx) = temp
  }

  def parentIndex(childIdx: Int): Int = {
    (childIdx - 1) / 2
  }

  def childIndicies(parentIdx: Int): (Option[Int], Option[Int]) = {
    val offset = (parentIdx * 2)
    val left   = offset + 1
    val right  = offset + 2
    (
      if (left < backing.length) Some(left) else None,
      if (right < backing.length) Some(right) else None
    )
  }

  def render(qty: Int= 10): String = {
    s"[${backing.slice(0, qty).mkString(", ")}]"

  }

  def draw() = {
    backing.foreach(v => print(s"$v, "))
  }
}
