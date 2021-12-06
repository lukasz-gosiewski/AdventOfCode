package com.gosiewski
package Day6

import scala.annotation.tailrec
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    println("Day 6")

    val daysPast = 80
    val initialFishList = Source.fromResource("input-6a.txt")
      .getLines()
      .toSeq
      .flatMap(_.split(","))
      .map(_.toInt)
      .map(fish => multiplyFishByTime(fish, daysPast))
      .sum

    println(initialFishList)
  }

  @tailrec
  def multiplyFishByTime(fish: Int, time: Int): Int = {
    if (time == 0) return 1
    else if (fish == 0) return multiplyFishByTime(6, time - 1) + multiplyFishByTime(8, time - 1)
    else multiplyFishByTime(fish - 1, time - 1)
  }
}
