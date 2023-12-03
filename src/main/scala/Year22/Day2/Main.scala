package com.gosiewski
package com.gosiewski.Year22.Day2

import scala.io.Source

object Main {

  def main(array: Array[String]): Unit = {
    println("Day 2")

    val resultA = solutionA("Year22/input-2.txt");
    val resultB = solutionB("Year22/input-2.txt");

    println(s"Variant A result: $resultA")
    println(s"Variant B result: $resultB")
  }

  def solutionA(fileName: String): Int = {
    val bufferedSource = Source.fromResource(fileName)

    var depth = 0
    var hposition = 0

    for (line <- bufferedSource.getLines()) {
      val tokenizedLine = line.split(" ").toList
      val command = tokenizedLine.head
      val commandValue = tokenizedLine(1).toInt

      command match {
        case "forward" => hposition += commandValue
        case "up" => depth -= commandValue
        case "down" => depth += commandValue
      }
    }

    bufferedSource.close

    depth * hposition
  }

  def solutionB(fileName: String): Int = {
    val bufferedSource = Source.fromResource(fileName)

    var depth = 0
    var hposition = 0
    var aim = 0

    for (line <- bufferedSource.getLines()) {
      val tokenizedLine = line.split(" ").toList
      val command = tokenizedLine.head
      val commandValue = tokenizedLine(1).toInt

      command match {
        case "forward" =>
          hposition += commandValue
          depth += aim * commandValue
        case "up" => aim -= commandValue
        case "down" => aim += commandValue
      }
    }

    bufferedSource.close

    depth * hposition
  }
}
