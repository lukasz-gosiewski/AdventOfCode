package com.gosiewski
package Day12

import scala.io.Source

object Day12 extends App {
  case class Node(name: String, isBig: Boolean)

  case class Connection(a: Node, b: Node)

  private val StartNode = Node("start", isBig = false)
  private val FinishNode = Node("end", isBig = false)

  val fileName = "input-12.txt"
  val nodes = getNodes(fileName)
  val connections = getConnections(fileName)
  val graph: Map[Node, Seq[Node]] = createGraph(nodes, connections)

  println(findPathsVisitingSmallCavesOnce(StartNode, Set.empty, graph))
  println(findPathsVisitingSmallCaveTwice(StartNode, Set.empty, Option.empty, graph))

  private def getNodes(fileName: String): Set[Node] =
    Source.fromResource(fileName)
      .getLines()
      .toSeq
      .flatMap(line => line.split("-"))
      .distinct
      .map(name => Node(name, isUppercase(name)))
      .toSet

  private def getConnections(fileName: String): Seq[Connection] =
    Source.fromResource(fileName)
      .getLines()
      .toSeq
      .map(line => {
        val splitLine = line.split("-")

        Connection(Node(splitLine(0), isUppercase(splitLine(0))), Node(splitLine(1), isUppercase(splitLine(1))))
      })

  private def createGraph(nodes: Set[Node], connections: Seq[Connection]): Map[Node, Seq[Node]] = {
    def findAdjacentNodes(a: Node, allConnections: Seq[Connection]): Seq[Node] = {
      val rightAdjacentNodes = allConnections.filter(connection => connection.a.equals(a))
        .map(connection => connection.b)

      val leftAdjacentNodes = allConnections.filter(connection => connection.b.equals(a))
        .map(connection => connection.a)

      rightAdjacentNodes ++ leftAdjacentNodes
    }

    nodes.map(node => node -> findAdjacentNodes(node, connections)).toMap
  }

  private def findPathsVisitingSmallCavesOnce(currentNode: Node, visitedNodes: Set[Node], graph: Map[Node, Seq[Node]]): Int = {
    if (currentNode.equals(FinishNode)) 1
    else if (!currentNode.isBig && visitedNodes.contains(currentNode)) 0
    else {
      var counter = 0
      for (adjacentNode <- graph(currentNode))
        counter += findPathsVisitingSmallCavesOnce(adjacentNode, visitedNodes + currentNode, graph)

      counter
    }
  }

  private def findPathsVisitingSmallCaveTwice(currentNode: Node, visitedNodes: Set[Node], dup: Option[Node], graph: Map[Node, Seq[Node]]): Int = {
    var newDup = dup
    if (currentNode.equals(FinishNode)) return 1
    if (currentNode.equals(StartNode) && visitedNodes.nonEmpty) return 0
    if (!currentNode.isBig && visitedNodes.contains(currentNode)) {
      if (dup.isEmpty) {
        newDup = Option(currentNode)
      }
      else return 0
    }

    var counter = 0
    for (thing <- graph(currentNode))
      counter += findPathsVisitingSmallCaveTwice(thing, visitedNodes + currentNode, newDup, graph)

    counter
  }

  private def isUppercase(string: String): Boolean = string.toUpperCase().equals(string)
}
