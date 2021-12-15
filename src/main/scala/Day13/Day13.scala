package com.gosiewski
package Day13


import Day13.Axis.Axis

import scala.io.Source

object Day13 extends App {
  object Axis extends Enumeration {
    type Axis = Value

    val X, Y = Value
  }

  case class Dot(x: Int, y: Int)
  case class Command(direction: Axis, line: Int)

  println("Day 13")
  val dots = getDots("input-13.txt")
  val foldCommands = getFoldCommands("input-13.txt")
  val foldedPaper = foldPaper(dots, foldCommands.head.direction, foldCommands.head.line)

  // How can I achieve the same, but with a functional way?
  var finalPaper = dots
  for (command <- foldCommands) {
    finalPaper = foldPaper(finalPaper, command.direction, command.line)
  }

  println(s"The amount of dots on the paper folded once: ${foldedPaper.length}")
  println(s"The amount of dots on the final paper: ${finalPaper.length}")
  println("The final grid after all folds:")

  for (x <- 0 to 49) {
    for (y <- 0 to 49) {
      if (finalPaper.contains(Dot(y, x))) print("#")
      else print(" ")
    }
    println()
  }


  def getDots(filename: String): Seq[Dot] =
    Source.fromResource(filename)
      .getLines()
      .toSeq
      .filter(line => line.split(",").length == 2)
      .map(line => Dot(line.split(",")(0).toInt, line.split(",")(1).toInt))

  def getFoldCommands(filename: String): Seq[Command] =
    Source.fromResource(filename)
      .getLines()
      .toSeq
      .filter(line => line.split(" ").length == 3 && line.split(" ")(0).equals("fold"))
      .map(line => {
        val splittedCommand = line.split(" ")
        val commandValue = splittedCommand(2).split("=")
        val axis = if (commandValue(0).equals("x")) Axis.X else Axis.Y

        Command(axis, commandValue(1).toInt)
      })

  def foldPaper(paper: Seq[Dot], axis: Axis, line: Int) = {
    if (axis.equals(Axis.X)) {
      paper.filter(dot => dot.x <= 2 * line)
        .map(dot => {
          val newX = if (dot.x > line) line - (dot.x - line) else dot.x

          Dot(newX, dot.y)
        })
        .distinct
    } else {
      paper.filter(dot => dot.y <= 2 * line)
        .map(dot => {
          val newY = if (dot.y > line) line - (dot.y - line) else dot.y

          Dot(dot.x, newY)
        })
        .distinct
    }
  }
}
