package com.gosiewski
package Day5

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Main {

  case class Point(x: Int, y: Int)
  case class Line(a: Point, b: Point)

  def main(args: Array[String]): Unit = {
    println("Day 5")

    val linesAsListsOfPoints = readLines("input-5.txt")
      .map(line => lineToListOfPoints(line))

    println(linesAsListsOfPoints.mkString(" \n"))

    val densityMap = linesAsListsOfPoints.flatten
      .groupBy(point => point)
      .mapValues(_.size)
      .toMap

    println(densityMap.count(entry => entry._2 > 1))
  }

  private def readLines(filename: String): Seq[Line] = {
    Source.fromResource(filename)
      .getLines()
      .map(textLine => parseLine(textLine))
      .toSeq
  }

  private def parseLine(string: String): Line = {
    val points = string.split(" -> ")
    val coordinatesA = points(0).split(",")
    val coordinatesB = points(1).split(",")
    val pointA = Point(coordinatesA(0).toInt, coordinatesA(1).toInt)
    val pointB = Point(coordinatesB(0).toInt, coordinatesB(1).toInt)

    Line(pointA, pointB)
  }


  // AAaaaaaaaa ifology
  private def lineToListOfPoints(line: Line): Seq[Point] = {
    val listOfPoints: ListBuffer[Point] = ListBuffer.empty

    if (line.a.x == line.b.x && line.a.y < line.b.y) {
      for (i <- line.a.y to line.b.y) {
        listOfPoints.addOne(Point(line.a.x, i))
      }
    } else if (line.a.x == line.b.x) {
      for (i <- line.b.y to line.a.y) {
        listOfPoints.addOne(Point(line.a.x, i))
      }
    } else if (line.a.y == line.b.y && line.a.x < line.b.x) {
      for (i <- line.a.x to line.b.x) {
        listOfPoints.addOne(Point(i, line.a.y))
      }
    } else if (line.a.y == line.b.y) {
      for (i <- line.b.x to line.a.x) {
        listOfPoints.addOne(Point(i, line.a.y))
      }
    } else if ((line.a.x - line.b.x).abs == (line.b.x - line.a.x).abs) {
      if (line.a.x < line.b.x && line.a.y < line.b.y) {
        for (i <- 0 to line.b.x - line.a.x) {
          listOfPoints.addOne(Point(line.a.x + i, line.a.y + i))
        }
      } else if (line.a.x < line.b.x) {
        for (i <- 0 to line.b.x - line.a.x) {
          listOfPoints.addOne(Point(line.a.x + i, line.a.y - i))
        }
      } else if (line.a.y < line.b.y) {
        for (i <- 0 to line.a.x - line.b.x) {
          listOfPoints.addOne(Point(line.a.x - i, line.a.y + i))
        }
      } else {
        for (i <- 0 to line.a.x - line.b.x) {
          listOfPoints.addOne(Point(line.a.x - i, line.a.y - i))
        }
      }
    }

    listOfPoints.toSeq
  }
}
