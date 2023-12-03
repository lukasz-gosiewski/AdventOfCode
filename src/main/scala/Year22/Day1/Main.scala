package com.gosiewski
package com.gosiewski.Year22.Day1

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    println("Day 1")

    val resultA = solutionA("Year22/input-1.txt")
    val resultB = solutionB("Year22/input-1.txt")
    println(s"Variant A result: $resultA")
    println(s"Variant B result: $resultB")
  }

  def solutionB(fileName: String): Int = {
    val bufferedSource = Source.fromResource(fileName)
    val slidingIterator = bufferedSource.getLines().sliding(3, 1)
    var previousNumber = slidingIterator.next().map(_.toInt).toList.sum
    var counter = 0

    for (lines <- slidingIterator) {
      val number = lines.map(_.toInt).toList.sum

      if (number > previousNumber)
         counter += 1

      previousNumber = number
    }
    bufferedSource.close

    counter
  }

  def solutionA(fileName: String): Int = {
    val bufferedSource = Source.fromResource(fileName)
    var counter = 0
    var previousNumber = bufferedSource.getLines().next().toInt

    for (line <- bufferedSource.getLines()) {

      if (line.toInt > previousNumber)
        counter += 1

      previousNumber = line.toInt
    }
    bufferedSource.close

    counter
  }

}
