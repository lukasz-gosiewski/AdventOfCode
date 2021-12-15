package com.gosiewski
package Day15

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day15 extends App {
  case class Node(x: Int, y: Int)
  type Graph[N] = N => Map[N, Int]

  val valueMap = getMap("input-15.txt")
  val tempGrid = (0 to 4).flatMap(k => valueMap.map(row => row.map(ele => if (ele + k < 10) ele + k else ele + k - 9)))
  val valueMap2 = tempGrid.map(line => (0 to 4).flatMap(k => line.map(ele => if (ele + k < 10) ele + k else ele + k - 9)))
  val graph1: Graph[Node] = node => {
    val tempMap: mutable.Map[Node, Int] = mutable.Map.empty
    if (node.x > 0) tempMap.addOne(Node(node.x - 1, node.y) -> valueMap(node.x - 1)(node.y))
    if (node.x < valueMap.length - 1) tempMap.addOne(Node(node.x + 1, node.y) -> valueMap(node.x + 1)(node.y))
    if (node.y > 0) tempMap.addOne(Node(node.x, node.y - 1) -> valueMap(node.x)(node.y - 1))
    if (node.y < valueMap.head.length - 1) tempMap.addOne(Node(node.x, node.y + 1) -> valueMap(node.x)(node.y + 1))

    tempMap.toMap
  }
  val graph2: Graph[Node] = node => {
    val tempMap: mutable.Map[Node, Int] = mutable.Map.empty
    if (node.x > 0) tempMap.addOne(Node(node.x - 1, node.y) -> valueMap2(node.x - 1)(node.y))
    if (node.x < valueMap2.length - 1) tempMap.addOne(Node(node.x + 1, node.y) -> valueMap2(node.x + 1)(node.y))
    if (node.y > 0) tempMap.addOne(Node(node.x, node.y - 1) -> valueMap2(node.x)(node.y - 1))
    if (node.y < valueMap2.head.length - 1) tempMap.addOne(Node(node.x, node.y + 1) -> valueMap2(node.x)(node.y + 1))

    tempMap.toMap
  }

  val result1 = shortestPath(graph1)(Node(0, 0), Node(99, 99)).get
    .filter(node => !node.equals(Node(0, 0)))
    .map(node => valueMap(node.x)(node.y))
    .sum
  val result2 = shortestPath(graph2)(Node(0, 0), Node(499, 499)).get
    .filter(node => !node.equals(Node(0, 0)))
    .map(node => valueMap2(node.x)(node.y))
    .sum

  println(s"Part one result: $result1")
  println(s"Part two result: $result2")

  def shortestPath[N](g: Graph[N])(source: N, target: N): Option[List[N]] = {
    val pred = dijkstra(g)(source)._2
    if (pred.contains(target) || source == target)
      Some(iterateRight(target)(pred.get))
    else None
  }

  def iterateRight[N](x: N)(f: N => Option[N]): List[N] = {
    @tailrec
    def go(x: N, acc: List[N]): List[N] = f(x) match {
      case None => x :: acc
      case Some(y) => go(y, x :: acc)
    }

    go(x, List.empty)
  }

  def dijkstra[N](g: Graph[N])(source: N): (Map[N, Int], Map[N, N]) = {
    @tailrec
    def go(active: Set[N], res: Map[N, Int], pred: Map[N, N]):
    (Map[N, Int], Map[N, N]) =
      if (active.isEmpty) (res, pred)
      else {
        val node = active.minBy(res)
        val cost = res(node)
        val neighbours = for {
          (n, c) <- g(node) if
            cost + c < res.getOrElse(n, Int.MaxValue)
        } yield n -> (cost + c)
        val active1 = active - node ++ neighbours.keys
        val preds = neighbours mapValues (_ => node)
        go(active1, res ++ neighbours, pred ++ preds)
      }

    go(Set(source), Map(source -> 0), Map.empty)
  }

  def getMap(filename: String): Seq[Seq[Int]] =
    Source.fromResource(filename)
      .getLines()
      .toSeq
      .map(line => line.toCharArray.toSeq)
      .map(row => row.map(risk => risk.asDigit))
}
