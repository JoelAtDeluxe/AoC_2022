package day16

import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

object Main {

  def main(args: Array[String]) = {
    val graph = new Graph()
    val parsedData = Source
      .fromResource("days11_20/day16_sample.txt")
      .getLines()
      .map(line => {
        """Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)""".r
          .findFirstMatchIn(line)
          .get
          .subgroups
      })
      .foreach(parts => {
        val node = Node(parts(0), parts(1).toInt)
        val conns = parts(2).split(",").map(_.trim()).toList
        graph.addNode(node, conns)
      })
    
    //println(graph.toString())
    val homeNode = graph.nodes("AA")
    val flowRateNodes = graph.nodes.values.filter( n => n.flowRate > 0).toList
    val nodesWeCareAbout = (
      (
        if (flowRateNodes.contains(homeNode)) null
        else homeNode
      ) +: flowRateNodes
    ).filter(n => n != null)

    
    println(graph)
    println("<-------->")
    
    val shadowGraph = new Graph()
    nodesWeCareAbout.foreach(n => {
      shadowGraph.addNode(n, List())
    })
    toWeightedGraph(graph, nodesWeCareAbout, shadowGraph, 1)
    println(shadowGraph)

    // val result = graph.minDistance("JJ", graph.edges("HH").toSet, Set(), 1)
    // println(s"${result.get} steps to connect AA and DD")

    println("Done!")

  }

  def magic(graph: Graph, position: String, db: List[Any]) = {
    
  }

  // def walk(
  //   graph: Graph,
  //   strategy: List[String],
  //   step: Int,
  //   curPos: String,
  //   turnsRemaining: Int,
  //   standbyPressure: Int,
  //   totalPressure: Int
  // ): Int = {
  //   if (turnsRemaining <= 0) {
  //     totalPressure
  //   }
  //   else {
  //     val to = strategy(step)
  //     val travelTime = graph.minDistance(curPos, to)
  //   }
  // }

  @tailrec
  def toWeightedGraph(graph: Graph, nodes: List[Node], result: Graph, openCost: Int): Graph = {
    if (nodes.length == 0) {
      result
    } else {
      val start = nodes.head
      val endpoints = nodes.tail

      endpoints.foreach(ep => {
        val dist = graph.minDistance(start.name, ep.name)
        result.addEdge(start.name, ep.name, dist.get + openCost) // assumes connected
      })
      toWeightedGraph(graph, nodes.tail, result, openCost)
    }
  }
}


// case class Node(name: String, flowRate: Int, connections: ListBuffer[Node])
// case class RawNode(name: String, flowRate: Int, connections: Array[String])
// case class Node(name: String, flowRate: Int)
