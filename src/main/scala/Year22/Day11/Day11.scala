package com.gosiewski
package com.gosiewski.Year22.Day11

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

// This time the program was made using a totally imperative and mutable approach
// Maybe it would be nice to try the same but with a proper, functional way?
object Day11 extends App {
  case class Octopus(x: Int, y: Int)

  println("Day 11")

  val result = performSimulationForExercise1(getInput)
  val result2 = performSimulationForExercise2(getInput)

  println(s"Count of flashes for 100 steps = $result")
  println(s"The first step to synchronize all octos: $result2")

  def getInput: Seq[ArrayBuffer[Int]] =
    Source.fromResource("Year22/input-11.txt")
      .getLines()
      .toSeq
      .map(line => line.toCharArray.toSeq.map(_.asDigit))
      .map(sss => ArrayBuffer(sss : _*))

  def simulateStep(matrix: Seq[ArrayBuffer[Int]]): (Seq[ArrayBuffer[Int]], Int) = {
    val flashed: ListBuffer[Octopus] = ListBuffer.empty

    def simulateOctopus(octo: Octopus): Unit = {
      matrix(octo.x)(octo.y) += 1

      if (matrix(octo.x)(octo.y) <= 9) return
      else if (flashed.contains(octo)) return
      else {
        flashed.addOne(octo)

        if (octo.x - 1 >= 0) simulateOctopus(Octopus(octo.x - 1, octo.y))
        if (octo.x + 1 < matrix.length) simulateOctopus(Octopus(octo.x + 1, octo.y))
        if (octo.y - 1 >= 0) simulateOctopus(Octopus(octo.x, octo.y - 1))
        if (octo.y + 1 < matrix.head.length) simulateOctopus(Octopus(octo.x, octo.y + 1))
        if (octo.x - 1 >= 0 && octo.y - 1 >= 0) simulateOctopus(Octopus(octo.x - 1, octo.y - 1))
        if (octo.x - 1 >= 0 && octo.y + 1 < matrix.head.length) simulateOctopus(Octopus(octo.x - 1, octo.y + 1))
        if (octo.x + 1 < matrix.length && octo.y - 1 >= 0) simulateOctopus(Octopus(octo.x + 1, octo.y - 1))
        if (octo.x + 1 < matrix.length && octo.y + 1 < matrix.head.length) simulateOctopus(Octopus(octo.x + 1, octo.y + 1))
      }
    }


    (0 until 10).foreach(x => {
      (0 until 10).foreach(y => {
        matrix(x)(y) += 1
      })
    })

    (0 until 10).foreach(x => {
      (0 until 10).foreach(y => {
        if (matrix(x)(y) > 9) simulateOctopus(Octopus(x, y))
      })
    })
    flashed.foreach(octo => matrix(octo.x)(octo.y) = 0)

    (matrix, flashed.length)
  }

  def performSimulationForExercise1(matrix: Seq[ArrayBuffer[Int]]): Int = {
    var currentMatrix = matrix
    var flashSum = 0

    (1 to 1000).foreach(i => {
      val result = simulateStep(matrix)
      currentMatrix = result._1
      flashSum += result._2
    })

    flashSum
  }

  def performSimulationForExercise2(matrix: Seq[ArrayBuffer[Int]]): Int = {
    var currentMatrix = matrix
    var step = 1

    while(true) {
      val result = simulateStep(matrix)
      currentMatrix = result._1

      if (result._2 == 100) return step

      step += 1
    }

    step
  }
}
