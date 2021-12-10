package com.gosiewski
package Day10

import scala.collection.mutable
import scala.io.Source

object Day10 extends App {

  private val checkersScoringMap: Map[Char, Int] = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137,
  )

  private val openingBracketsMap: Map[Char, Char] = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  private val autocompleteScoringMap: Map[Char, Int] = Map(
    '(' -> 1,
    '[' -> 2,
    '{' -> 3,
    '<' -> 4,
  )

  private val exercise1Score = getInput.filter(line => !isCorrect(line))
    .map(line => mapIncorrectLineToScore(line, checkersScoringMap))
    .sum

  private val exercise2PartialScores = getInput.filter(line => isCorrect(line))
    .map(line => mapIncompleteLineToScore(line, autocompleteScoringMap))
    .sorted
  private val exercise2FinalScore = exercise2PartialScores(exercise2PartialScores.length / 2)

  println(s"Exercise 1 score: $exercise1Score")
  println(s"Exercise 2 score: $exercise2FinalScore")

  private def getInput: Seq[Seq[Char]] =
    Source.fromResource("input-10.txt")
      .getLines
      .toSeq
      .map(line => line.toCharArray.toSeq)

  private def isCorrect(line: Seq[Char]): Boolean = {
    val stack: mutable.Stack[Char] = mutable.Stack()

    line.forall(character => {
      if (openingBracketsMap.contains(character)) {
        stack.push(character)

        true
      }
      else if (character == openingBracketsMap(stack.pop())) true
      else false
    })
  }

  private def mapIncorrectLineToScore(line: Seq[Char], scoringMap: Map[Char, Int]): BigInt = {
    val stack: mutable.Stack[Char] = mutable.Stack()

    line.foreach(character => {
      if (openingBracketsMap.contains(character)) stack.push(character)
      else if (character != openingBracketsMap(stack.pop())) return scoringMap(character)
    })

    BigInt(0L)
  }

  private def mapIncompleteLineToScore(line: Seq[Char], matchingScoringMap: Map[Char, Int]): BigInt = {
    val stack: mutable.Stack[Char] = mutable.Stack()

    line.foreach(character => {
      if (openingBracketsMap.contains(character)) stack.push(character)
      else {
        val stackElement = stack.pop()

        if (character != openingBracketsMap(stackElement)) stack.push(stackElement)
      }
    })

    stack.toSeq
      .map(character => matchingScoringMap(character))
      .foldLeft(BigInt(0L))((score, partialScore) => score * 5L + partialScore)
  }
}
