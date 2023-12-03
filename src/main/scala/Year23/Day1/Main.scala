package com.gosiewski
package Year23.Day1

import java.lang.Character.isDigit
import scala.io.Source

object Main {
    def main(args: Array[String]): Unit = {
        println(solutionA("2023/input-1.txt"))
    }

    def solutionA(filename: String): Int = {
        val lines = Source.fromResource(filename).getLines()

        lines.map(line => mapLineToNumber(line))
            .sum
    }

    def mapLineToNumber(line: String): Int = {
        val numbers = line.toCharArray.filter(s => isDigit(s))

        s"${numbers.head}${numbers.last}".toInt
    }
}
