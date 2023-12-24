package day16

import scala.collection.mutable
import scala.annotation.tailrec

case class Node(name: String, flowRate: Int)

class Graph() {
  val nodes: mutable.Map[String, Node]         = mutable.Map()// list?
  val edges: mutable.Map[String, List[Edge]]   = mutable.Map()
  val distances: mutable.Map[Edge, Int]        = mutable.Map()

  def addNode(node: Node, edges: List[String]) {
    this.nodes(node.name) = node
    this.edges(node.name) = edges.map(endP => Edge(node.name, endP))
  }

  def addEdge(start: String, end: String, weight: Int) = {
    edges(start) = Edge(start, end, weight) +: edges(start)
    edges(end) = Edge(end, start, weight) +: edges(end)
  }

  override def toString(): String = {
    this.nodes.values
      .map(n => {
        val prefix  = s"${n.name}{${n.flowRate}}"
        val postfix = edges.get(n.name).get.map(c => s"-> ${c}").mkString(" ")
        s"${prefix} : ${postfix}"
      })
      .mkString("\n")
  }

  final def minDistance(from: String, to: String): Option[Int] = {
    minDistance(to, this.edges(from).toSet, Set(), 1)
  }

  @tailrec
  final def minDistance(target: String, toCheck: Set[Edge], seen: Set[String], steps: Int): Option[Int] = {
    if (toCheck.isEmpty) None
    else {
      val placesToCheck = toCheck.map(edge => edge.to)
      val possibleRoutes = placesToCheck.filter(_ == target)

      if (possibleRoutes.size > 0) {
        Some(steps)
      }
      else {
        val newSeen = (placesToCheck ++ seen)
        // build up the next check
        val newToCheck = placesToCheck
          .map(p => edges.getOrElse(p, List()))
          .flatten
          .toSet
        minDistance(target, newToCheck, newSeen, steps+1)
      }
    }
  }

  // def minimizeDistance() = {
  //   // set base paths
  //   for (nodeName <- nodes.keys) {
  //     val paths = edges(nodeName).map(to => Edge(nodeName, to))
  //     paths.foreach(p => { distances(p) = 1 })
  //   }
  // }
}

object Graph {

  def repath(aPath: Edge, b: String) = {}
}

case class Edge(from: String, to: String, weight: Int=1)
