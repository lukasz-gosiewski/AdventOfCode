package com.gosiewski
package com.gosiewski.Year22.Day20

import scala.io.Source

object Day20 extends App {

  def getEnhancementString(filename: String): String =
    Source.fromResource(filename)
      .getLines()
      .next()

  def getInputImage(filename: String): Seq[Seq[Char]] =
    Source.fromResource(filename)
      .getLines()
      .toSeq
      .drop(2)
      .map(_.toCharArray.toSeq)

  println("Day 20")


  def translateToNewPixel(inputPixels: Seq[Seq[Char]], enhancementString: String): Char = {
    val index = Integer.valueOf(inputPixels.flatMap(row => row.map(char => if (char.equals('#')) 1 else 0))
      .toString(), 2)

    enhancementString(index)
  }

  
}
