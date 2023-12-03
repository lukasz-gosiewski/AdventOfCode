package com.gosiewski
package com.gosiewski.Year22.Day17

import scala.io.Source

// Well, if it works, it works. Looks ugly, but whatever
object Day17 extends App {
  case class TargetArea(startX: Int, endX: Int, startY: Int, endY: Int)
  case class Probe(xVelocity: Int, xPosition: Int, yVelocity: Int, yPosition: Int)

  println("Day 17")

  val targetArea = getTargetArea("Year22/input-17.txt")
  println(targetArea)
  println(calculateHighestPossiblePosition(targetArea))
  println(getAllCorrectVelocities(targetArea).length)

  private def calculateHighestPossiblePosition(targetArea: TargetArea): Int = {
    val lowestYTargetPoint = targetArea.startY

    ((Math.abs(lowestYTargetPoint) - 1) * Math.abs(lowestYTargetPoint)) / 2
  }

  private def getAllCorrectVelocities(targetArea: TargetArea): Seq[(Int, Int)] = {
    val allPossibleValues = for {
      x <- -1000 to 1000
      y <- -1000 to 1000
    } yield (x, y)

    allPossibleValues.filter(pair => willLandInTarget(pair._1, pair._2, targetArea))
  }

  private def willLandInTarget(velX: Int, velY: Int, targetArea: TargetArea): Boolean = {
    var positionX = 0
    var positionY = 0
    var XVelocity = velX
    var YVelocity = velY

    while (positionX <= targetArea.endX && positionY >= targetArea.startY) {
      if (positionX >= targetArea.startX && positionY <= targetArea.endY) return true

      positionX += XVelocity
      positionY += YVelocity

      XVelocity = if (XVelocity > 0) XVelocity - 1 else if (XVelocity < 0) XVelocity + 1 else XVelocity
      YVelocity = YVelocity - 1
    }

    false
  }

  private def getTargetArea(filename: String): TargetArea =
    Source.fromResource(filename)
      .getLines()
      .toSeq
      .map(line => {
        val coors = line.slice(13, line.length)
        val xCoors = coors.split(", ")(0).drop(2)
        val yCoors = coors.split(", ")(1).drop(2)

        val startX = xCoors.split("\\.\\.")(0)
        val endX = xCoors.split("\\.\\.")(1)

        val startY = yCoors.split("\\.\\.")(0)
        val endY = yCoors.split("\\.\\.")(1)

        TargetArea(startX.toInt, endX.toInt, startY.toInt, endY.toInt)
      })
      .head


}
