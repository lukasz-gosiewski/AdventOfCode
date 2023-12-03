package com.gosiewski
package com.gosiewski.Year22.Day9

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Main extends App {

  case class LowPoint(x: Int, y: Int, height: Int)

  val floormap = getFloorMapArray("Year22/input-9.txt")
  val lowpoints = findLowPoints(floormap)
  val basins = findBasinsSizes(lowpoints, floormap)
  val result = calculateRisk(lowpoints)
  val result2 = basins.sorted.reverse.take(3).product

  println(basins)

  println("Day 9")
  println(s"Solution a: $result")
  println(s"Solution b: $result2")

  private def getFloorMapArray(filename: String): Array[Array[Int]] = {
    Source.fromResource(filename)
      .getLines()
      .toSeq
      .map(_.toCharArray)
      .map(_.map(_.asDigit))
      .toArray
  }

  private def findLowPoints(floorMap: Array[Array[Int]]): Seq[LowPoint] = {
    val result: ListBuffer[LowPoint] = mutable.ListBuffer.empty

    for (i <- floorMap.indices) {
      for (j <- floorMap(i).indices) {
        if (j - 1 >= 0 && floormap(i)(j) >= floormap(i)(j - 1)) result :+ Seq.empty
        else if (i - 1 >= 0 && floormap(i)(j) >= floormap(i - 1)(j)) result :+ Seq.empty
        else if (j + 1 < floormap(i).length && floormap(i)(j) >= floormap(i)(j + 1)) result :+ Seq.empty
        else if (i + 1 < floormap.length && floormap(i)(j) >= floormap(i + 1)(j)) result :+ Seq.empty
        else result += LowPoint(i, j, floorMap(i)(j))
      }
    }

    result.toSeq
  }

  private def findBasinsSizes(lowpoints: Seq[LowPoint], floorMap: Array[Array[Int]]): Seq[Int] = {
    lowpoints.map(point => {
      val basin: mutable.ListBuffer[LowPoint] = ListBuffer.empty
      findBasin(point, floorMap, basin)

      basin
    })
      .map(points => points.length)
  }

  private def findBasin(lowPoint: LowPoint, matrix: Array[Array[Int]], basin: mutable.ListBuffer[LowPoint]): Unit = {
    if (lowPoint.height == 9) return
    else if (basin.contains(lowPoint)) return
    else {
      basin.addOne(lowPoint)

      val x = lowPoint.x
      val y = lowPoint.y

      if (y - 1 >= 0 && matrix(x)(y) < matrix(x)(y - 1)) findBasin(LowPoint(x, y - 1, matrix(x)(y - 1)), matrix, basin)
      if (x - 1 >= 0 && matrix(x)(y) < matrix(x - 1)(y)) findBasin(LowPoint(x - 1, y, matrix(x - 1)(y)), matrix, basin)
      if (y + 1 < matrix(x).length && matrix(x)(y) < matrix(x)(y + 1)) findBasin(LowPoint(x, y + 1, matrix(x)(y + 1)), matrix, basin)
      if (x + 1 < matrix.length && matrix(x)(y) < matrix(x + 1)(y)) findBasin(LowPoint(x + 1, y, matrix(x + 1)(y)), matrix, basin)
    }
  }

  private def calculateRisk(lowpoints: Seq[LowPoint]): Int = {
    lowpoints.map(x => x.height + 1)
      .sum
  }
}
