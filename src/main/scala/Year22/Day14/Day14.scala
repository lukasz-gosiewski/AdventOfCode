package com.gosiewski
package com.gosiewski.Year22.Day14

import scala.collection.mutable
import scala.io.Source

object Day14 extends App {

  println("Day 14")

  val polymerTemplate = getPolymerTemplate("Year22/input-14.txt")
  val insertionRules = getInsertionRules("Year22/input-14.txt")

  var elementsOccurMap1 = polymerTemplate.groupBy(identity).mapValues(group => BigInt(group.length)).toMap
  var resultMap1 = createOccurrenceMap(polymerTemplate)
  (0 until 10).foreach(_ => {
    val result = performStep(resultMap1, elementsOccurMap1, insertionRules)
    resultMap1 = result._1
    elementsOccurMap1 = result._2
  })

  var elementsOccurMap2 = polymerTemplate.groupBy(identity).mapValues(group => BigInt(group.length)).toMap
  var resultMap2 = createOccurrenceMap(polymerTemplate)
  (0 until 40).foreach(_ => {
    val result = performStep(resultMap2, elementsOccurMap2, insertionRules)
    resultMap2 = result._1
    elementsOccurMap2 = result._2
  })

  val result1 = elementsOccurMap1.maxBy(_._2)._2 - elementsOccurMap1.minBy(_._2)._2
  val result2 = elementsOccurMap2.maxBy(_._2)._2 - elementsOccurMap2.minBy(_._2)._2

  println(s"Result 1: $result1")
  println(s"Result 2: $result2")

  def getPolymerTemplate(filename: String): Seq[Char] =
    Source.fromResource(filename)
      .getLines()
      .toSeq
      .head.toCharArray

  def getInsertionRules(filename: String): Map[(Char, Char), Char] =
    Source.fromResource(filename)
      .getLines()
      .toSeq
      .drop(2)
      .map(line => {
        val from = line.split(" -> ")(0).toCharArray

        (from(0), from(1)) -> line.split(" -> ")(1).charAt(0)
      }).toMap

  def createOccurrenceMap(init: Seq[Char]): Map[(Char, Char), BigInt] = {
    val tempMap: mutable.Map[(Char, Char), BigInt] = mutable.Map.empty

    init.sliding(2).foreach(window => {
      val pair = (window(0), window(1))
      if (tempMap.contains(pair)) tempMap(pair) += 1
      else tempMap.put(pair, 1)
    })

    tempMap.toMap
  }

  def performStep(occurencesMap: Map[(Char, Char), BigInt], elementsOccurencesMap: Map[Char, BigInt], insertionRules: Map[(Char, Char), Char]): (Map[(Char, Char), BigInt], Map[Char, BigInt]) = {
    val tempMap: mutable.Map[(Char, Char), BigInt] = mutable.Map.empty
    val tempElementsMap = mutable.Map(elementsOccurencesMap.toSeq: _*)

    occurencesMap.foreach(pair => {
      if (insertionRules.contains(pair._1)) {
        val newPair1 = (pair._1._1, insertionRules(pair._1))
        val newPair2 = (insertionRules(pair._1), pair._1._2)

        if (tempMap.contains(newPair1)) tempMap(newPair1) += pair._2
        else tempMap.put(newPair1, pair._2)

        if (tempMap.contains(newPair2)) tempMap(newPair2) += pair._2
        else tempMap.put(newPair2, pair._2)

        if (tempElementsMap.contains(insertionRules(pair._1))) tempElementsMap(insertionRules(pair._1)) += pair._2
        else tempElementsMap.put(insertionRules(pair._1), pair._2)
      }
    })

    (tempMap.toMap, tempElementsMap.toMap)
  }
}
