package com.gosiewski
package Day18


import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

object Day18 extends App {
  trait SnailFishNumber
  case class SnailFishPair(left: SnailFishNumber, right: SnailFishNumber) extends SnailFishNumber
  case class SnailFishValue(value: Int) extends SnailFishNumber

  object SnailFishNumberParser extends RegexParsers {
    def snailFishNumber: Parser[SnailFishNumber] = snailFishValue | snailFishPair
    def snailFishValue: Parser[SnailFishValue] = """\d+""".r ^^ { valueStr =>  SnailFishValue(valueStr.toInt) }
    def snailFishPair: Parser[SnailFishPair] =
      literal("[") ~ snailFishNumber ~ literal(",") ~ snailFishNumber ~ literal("]") ^^ {
        case _ ~ left ~ _ ~ right ~ _ => SnailFishPair(left, right)
      }
  }

  println("Day 18")

  val input = getInput("input-18.txt")



  def getInput(filename: String): Seq[SnailFishNumber] =
    Source.fromResource(filename)
      .getLines
      .toSeq
      .map(line => parseSnailFishNumber(line))

  def parseSnailFishNumber(numberString: String): SnailFishNumber =
    SnailFishNumberParser.parse(SnailFishNumberParser.snailFishNumber, numberString).get

  def addSnailFishNumbers(left: SnailFishNumber, right: SnailFishNumber): SnailFishNumber =
    SnailFishPair(left, right)

  def reduceSnailFishNumber(number: SnailFishNumber) = {
    
  }

  /**
  def getRealValue(snailNumber: SnailNumber): Either[Exception, Int] = {
    snailNumber match {
      case SnailPair(left, right) => for (leftValue <- getRealValue(left); rightValue <- getRealValue(right)) yield leftValue + rightValue
      case SnailValue(value) => Right(value)
      case _ => Left(new IllegalArgumentException("Unsupported operation"))
    }
  }
   */
}
