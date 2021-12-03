package com.gosiewski
package Day1

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    println("Day 1")

    val bufferedSource = Source.fromResource("input-1-1.txt")
    var counter = 0
    var previousNumber = bufferedSource.getLines().next().toInt

    for (line <- bufferedSource.getLines()) {

      if (line.toInt > previousNumber)
        counter += 1

      previousNumber = line.toInt
    }

    println(s"The answer is: $counter")

    bufferedSource.close
  }

}
