package com.gosiewski
package com.gosiewski.Year22.Day6

import scala.collection.mutable
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    println("Day 6")

    val daysPast = 256
    val initialFishList = Source.fromResource("Year22/input-6.txt")
      .getLines()
      .toSeq
      .flatMap(_.split(","))
      .map(_.toInt)

    val counters = mutable.ArrayBuffer(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)

    initialFishList.foreach(fish => counters(fish) += 1)

    for (_ <- 0 until daysPast) {
      val newBorns = counters(0)

      for (i <- 0 to 7) {
        counters(i) = counters(i + 1)
      }

      counters(8) = newBorns
      counters(6) += newBorns
    }

    println(counters.sum)
  }
}
