package com.gosiewski
package Day3

import scala.io.Source

object Main {
  /**
   * 011110111101 OX
   * 011110010000 OX
   *
   * 110100101011 CO
   * 110100101011 CO
   */

  def main(args: Array[String]): Unit = {
    println("Day 3")

    val resultA = solutionA("input-3.txt")
    val resultB = solutionB("input-3.txt")

    println(s"Variant A result: $resultA")
    println(s"Variant B result: $resultB")

    println(getOxGenRating("input-3.txt"))
    println(getCoRating("input-3.txt"))
  }

  def solutionA(fileName: String): Int = {
    val gamma = Integer.parseInt(getCommonBitsString(readFile(fileName)), 2)
    val epsilon = Integer.parseInt(getLeastCommonBitsString(readFile(fileName)), 2)

    gamma * epsilon
  }

  def solutionB(fileName: String): Int = {
    val oxGenRating = Integer.parseInt(getOxGenRating(fileName), 2)
    val coRating = Integer.parseInt(getCoRating(fileName), 2)

    println(s"OxGenRating: $oxGenRating, CoRating: $coRating")

    oxGenRating * coRating
  }

  def getOxGenRating(fileName: String): String = {
    var filteredInput = readFile(fileName)

    for (i <- 0 until 12) {
      val commonBits = getCommonBitsString(filteredInput)
      filteredInput = filteredInput.filter(line => line(i).equals(commonBits(i)))

      if (filteredInput.length == 1) return filteredInput.head
    }

    throw new IllegalArgumentException("Bit criteria too weak - could not determine the single number")
  }

  def getCoRating(fileName: String): String = {
    var filteredInput = readFile(fileName)

    for (i <- 0 until 12) {
      val leastCommonBits = getLeastCommonBitsString(filteredInput)
      filteredInput = filteredInput.filter(line => line(i).equals(leastCommonBits(i)))

      if (filteredInput.length == 1) return filteredInput.head
    }

    throw new IllegalArgumentException("Bit criteria too weak - could not determine the single number")
  }

  def getCommonBitsString(input: Seq[String]): String = {
    val resultArray = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    for (line <- input) {
      val characters = line.toCharArray

      for (i <- 0 until characters.length) {
        characters(i) match {
          case '0' => resultArray(i) -= 1
          case '1' => resultArray(i) += 1
        }
      }
    }

    var resultString = ""
    for (number <- resultArray) {
      if (number >= 0) {
        resultString = resultString + "1"
      } else if (number < 0) {
        resultString = resultString + "0"
      }
    }

    resultString
  }

  def getLeastCommonBitsString(input: Seq[String]): String = {
    val resultArray = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    for (line <- input) {
      val characters = line.toCharArray

      for (i <- 0 until characters.length) {
        characters(i) match {
          case '0' => resultArray(i) -= 1
          case '1' => resultArray(i) += 1
        }
      }
    }

    var resultString = ""
    for (number <- resultArray) {
      if (number >= 0) {
        resultString = resultString + "0"
      } else if (number < 0) {
        resultString = resultString + "1"
      }
    }

    resultString
  }

  def readFile(filename: String): Seq[String] = {
    val bufferedSource = Source.fromResource(filename)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
    bufferedSource.close

    lines
  }
}
