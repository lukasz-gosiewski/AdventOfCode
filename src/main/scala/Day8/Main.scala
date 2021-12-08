package com.gosiewski
package Day8

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    println("Day 8")

    val notes = Source.fromResource("input-8.txt")
      .getLines()
      .toSeq
      .map(_.split(" \\| "))
      .map(array => (array(0), array(1)))
      .map(a => (a._1.split(" "), a._2.split(" ")))

    println(countUniqueOutputs(notes.map(_._2)))
    println(translateOutput(notes).sum)
  }

  def countUniqueOutputs(outputs: Seq[Array[String]]): Int = {
    outputs.flatten
      .count(output => output.length == 2 || output.length == 4 || output.length == 3 || output.length == 7)
  }

  def translateOutput(notes: Seq[(Array[String], Array[String])]): Seq[Int] = {
    notes
      .map(line => translateLine(line._1, line._2))
  }

  def translateLine(inputs: Seq[String], output: Array[String]): Int = {
    val uniqueMap = inputs.map(codedDigit => {
      if (codedDigit.length == 2) 1 -> codedDigit
      else if (codedDigit.length == 4) 4 -> codedDigit
      else if (codedDigit.length == 3) 7 -> codedDigit
      else if (codedDigit.length == 7) 8 -> codedDigit
      else -1 -> codedDigit
    }).toMap

    val result = output.map(codedDigit => {
        if (codedDigit.length == 2) 1
        else if (codedDigit.length == 4) 4
        else if (codedDigit.length == 3) 7
        else if (codedDigit.length == 7) 8
        else if (codedDigit.length == 6) {
          if (uniqueMap(1).intersect(codedDigit).length != 2) 6
          else if (uniqueMap(4).intersect(codedDigit).length != 4) 0
          else 9
        } else if (codedDigit.length == 5) {
          if (uniqueMap(1).intersect(codedDigit).length == 2) 3
          else if (uniqueMap(4).intersect(codedDigit).length == 3) 5
          else 2
        } else throw new IllegalArgumentException("Cannot decode output")
      })

    result.mkString.toInt
  }
}

