package com.gosiewski
package com.gosiewski.Year22.Day7

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    println("Day 7")

    val crabsPositions = Source.fromResource("Year22/input-7.txt")
      .getLines()
      .next()
      .split(",")
      .map(_.toInt)

    val result = (0 to crabsPositions.max).map(position => {
      crabsPositions.map(crabsPosition => (position - crabsPosition).abs)
        .sum
    }).min

    val result2 = (0 to crabsPositions.max).map(position => {
      crabsPositions.map(crabsPosition => (0 to (crabsPosition - position).abs).sum)
        .sum
    }).min

    println(result)
    println(result2)
  }
}
