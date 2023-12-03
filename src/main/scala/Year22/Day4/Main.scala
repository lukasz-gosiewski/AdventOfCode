package com.gosiewski
package com.gosiewski.Year22.Day4

import scala.io.Source

// This whole Day should be rebuild to use a pair of boards for each board - one with numbers and one with booleans
object Main {

  def main(args: Array[String]): Unit = {
    println("Day 4")

    val filename = "Year22/input-4.txt"
    val winningNumbers = readWinningNumbers(filename)
    val boards = readBoards(filename)

    val winningBoard = findWinningBoard(winningNumbers, boards)
    val winningScore = winningBoard.map(board => calculateScore(board._1, winningNumbers.take(board._2)))

    println(s"The winning numbers: ${winningNumbers.mkString(" ")}")

    println(s"The winning board:")
    if (winningBoard.isDefined) println(winningBoard.get._1.map(_.mkString(" ")).mkString("\n"))
    else println("None")
    println(s"Score of a winning board: $winningScore")

    val lastToWinBoard = findLastToWinBoard(winningNumbers, boards)
    val lastToWinBoardScore = lastToWinBoard.map(board => calculateScore(board._1, winningNumbers.take(board._2)))

    println(s"The last to win board:")
    if (lastToWinBoardScore.isDefined) println(lastToWinBoard.get._1.map(_.mkString(" ")).mkString("\n"))
    else println("None")
    println(s"Score of a last to win board: $lastToWinBoardScore")
  }

  private def readWinningNumbers(fileName: String): Seq[Int] = {
    Source.fromResource(fileName)
      .getLines()
      .next()
      .split(",")
      .toSeq
      .map(_.toInt)
  }

  private def readBoards(fileName: String): Seq[Array[Array[Int]]] = {
    Source.fromResource(fileName)
      .getLines()
      .drop(2)
      .filter(_.nonEmpty)
      .grouped(5)
      .toSeq
      .map(_.map(_.split(" ").filter(_.nonEmpty).map(_.toInt).array).toArray)
  }

  private def findWinningBoard(winningNumbers: Seq[Int], allBoards: Seq[Array[Array[Int]]]): Option[(Array[Array[Int]], Int)] = {
    for (i <- 5 to winningNumbers.size) {
      for (board <- allBoards) {
        if (checkIfWon(winningNumbers.take(i).toArray, board))
          return Some(board, i)
      }
    }

    None
  }

  private def findLastToWinBoard(winningNumbers: Seq[Int], allBoards: Seq[Array[Array[Int]]]): Option[(Array[Array[Int]], Int)] = {
    var remainingBoards = allBoards

    for (i <- 5 to winningNumbers.size) {
      for (board <- remainingBoards) {
        if (checkIfWon(winningNumbers.take(i).toArray, board)) {
          if (remainingBoards.length == 1) return Some((remainingBoards.head, i))

          remainingBoards = remainingBoards.filterNot(element => element.equals(board))
        }
      }
    }

    None
  }

  private def checkIfWon(fetchedNumbers: Array[Int], board: Array[Array[Int]]): Boolean = {
    for (hline <- board) {
      if (hline.count(element => fetchedNumbers.contains(element)) == 5) return true
    }

    for (vline <- board.clone().transpose) {
      if (vline.count(element => fetchedNumbers.contains(element)) == 5) return true
    }

    false
  }

  private def calculateScore(board: Array[Array[Int]], fetchedNumbers: Seq[Int]): Int = {
    var sumOfNotCrossedElements = 0
    for (row <- board) {
      for (item <- row) {
        if (!fetchedNumbers.contains(item)) sumOfNotCrossedElements += item
      }
    }

    sumOfNotCrossedElements * fetchedNumbers.last
  }
}
