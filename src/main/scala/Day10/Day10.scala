package com.gosiewski
package Day10

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App {

  private val openingBracketsMap: Map[Char, Char] = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  {
    val checkersScoringMap: Map[Char, Int] = Map(
      ')' -> 3,
      ']' -> 57,
      '}' -> 1197,
      '>' -> 25137,
    )

    val autocompleteScoringMap: Map[Char, Int] = Map(
      ')' -> 1,
      ']' -> 2,
      '}' -> 3,
      '>' -> 4,
    )

    val exercise1Score = getInput.filter(line => !isCorrect(line))
      .map(line => mapIncorrectLineToScore(line, checkersScoringMap))
      .sum

    val exercise2PartialScores = getInput.filter(line => isCorrect(line))
      .map(line => mapIncompleteLineToScore(line, autocompleteScoringMap))
      .sorted

    val exercise2FinalScore = exercise2PartialScores(exercise2PartialScores.length / 2)

    println(s"Exercise 1 score: $exercise1Score")
    println(s"Exercise 2 score: $exercise2FinalScore")
  }

  private def getInput: Seq[Seq[Char]] =
    Source.fromResource("input-10.txt")
      .getLines
      .toSeq
      .map(line => line.toCharArray.toSeq)

  private def isCorrect(line: Seq[Char]): Boolean = {

    @tailrec
    def checkRecursive(sequence: Seq[Char], openBrackets: Seq[Char]): Boolean = {
      if (sequence.isEmpty) true
      else if (openingBracketsMap.contains(sequence.head)) checkRecursive(sequence.tail, openBrackets :+ sequence.head)
      else if (sequence.head == openingBracketsMap(openBrackets.last)) checkRecursive(sequence.tail, openBrackets.dropRight(1))
      else false
    }

    checkRecursive(line, Seq.empty)
  }

  private def mapIncorrectLineToScore(line: Seq[Char], scoringMap: Map[Char, Int]): BigInt = {

    @tailrec
    def mapRecursive(sequence: Seq[Char], openBrackets: Seq[Char], scoringMap: Map[Char, Int]): BigInt = {
      if (sequence.isEmpty) 0
      else if (openingBracketsMap.contains(sequence.head)) mapRecursive(sequence.tail, openBrackets :+ sequence.head, scoringMap)
      else if (sequence.head == openingBracketsMap(openBrackets.last)) mapRecursive(sequence.tail, openBrackets.dropRight(1), scoringMap)
      else scoringMap(sequence.head)
    }

    mapRecursive(line, Seq.empty, scoringMap)
  }

  private def mapIncompleteLineToScore(line: Seq[Char], scoringMap: Map[Char, Int]): BigInt = {

    @tailrec
    def mapIncompleteLineToItsCompletion(sequence: Seq[Char], lackingBrackets: Seq[Char]): Seq[Char] = {
      if (sequence.isEmpty) lackingBrackets
      else if (openingBracketsMap.contains(sequence.head)) mapIncompleteLineToItsCompletion(sequence.tail, lackingBrackets :+ openingBracketsMap(sequence.head))
      else if (sequence.head == lackingBrackets.last) mapIncompleteLineToItsCompletion(sequence.tail, lackingBrackets.dropRight(1))
      else throw new IllegalArgumentException("Line is incorrect")
    }

    mapIncompleteLineToItsCompletion(line, Seq.empty)
      .map(character => scoringMap(character))
      .foldLeft(BigInt(0L))((score, partialScore) => score * 5L + partialScore)
  }
}
