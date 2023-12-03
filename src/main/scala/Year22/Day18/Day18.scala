package com.gosiewski
package com.gosiewski.Year22.Day18


import scala.annotation.tailrec
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

object Day18 extends App {
  sealed trait SnailFishNumber
  case class SnailFishPair(left: SnailFishNumber, right: SnailFishNumber) extends SnailFishNumber
  case class SnailFishValue(value: Int) extends SnailFishNumber

  sealed trait ExplosionResult { def value: SnailFishNumber }
  final case class NotExploded(value: SnailFishNumber) extends ExplosionResult
  final case class Exploded(value: SnailFishNumber) extends ExplosionResult
  final case class JustExploded(value: SnailFishNumber, leftAdd: Int, rightAdd: Int) extends ExplosionResult
  final case class LeftExploded(value: SnailFishNumber, add: Int) extends ExplosionResult
  final case class RightExploded(value: SnailFishNumber, add: Int) extends ExplosionResult

  sealed trait SplitResult
  final case class Split(value: SnailFishNumber) extends SplitResult
  final case class NotSplit(value: SnailFishNumber) extends SplitResult

  object SnailFishNumberParser extends RegexParsers {
    def snailFishNumber: Parser[SnailFishNumber] = snailFishValue | snailFishPair
    def snailFishValue: Parser[SnailFishValue] = """\d+""".r ^^ { valueStr =>  SnailFishValue(valueStr.toInt) }
    def snailFishPair: Parser[SnailFishPair] =
      literal("[") ~ snailFishNumber ~ literal(",") ~ snailFishNumber ~ literal("]") ^^ {
        case _ ~ left ~ _ ~ right ~ _ => SnailFishPair(left, right)
      }
  }

  val input = getInput("Year22/input-18.txt")
  val sumAll = input.reduceLeft((a, b) => add(a, b))
  val sumMagnitude = calculateMagnitude(sumAll)
  val allCombinationsOf2 = for {
    left <- input
    right <- input.filterNot(_ == left)
  } yield (left, right)
  val biggestMagnitudeOfSumOf2 = allCombinationsOf2.map(combination => calculateMagnitude(add(combination._1, combination._2))).max

  println("Day 18")
  println(s"The sum of all numbers: $sumAll")
  println(s"The magnitude of this sum: $sumMagnitude")
  println(s"The biggest possible magnitude of sum of 2 numbers from the list: $biggestMagnitudeOfSumOf2")

  def getInput(filename: String): Seq[SnailFishNumber] =
    Source.fromResource(filename)
      .getLines
      .toSeq
      .map(line => parseSnailFishNumber(line))

  def parseSnailFishNumber(numberString: String): SnailFishNumber =
    SnailFishNumberParser.parse(SnailFishNumberParser.snailFishNumber, numberString).get

  def add(left: SnailFishNumber, right: SnailFishNumber): SnailFishNumber =
    reduce(SnailFishPair(left, right))

  @tailrec
  def reduce(number: SnailFishNumber): SnailFishNumber = {
    explode(number, 0) match {
      case NotExploded(value) =>
        split(value) match {
          case Split(value)    => reduce(value)
          case NotSplit(value) => value
        }
      case exploded => reduce(exploded.value)
    }
  }

  def explode(number: SnailFishNumber, depth: Int): ExplosionResult = number match {
    case SnailFishPair(SnailFishValue(left), SnailFishValue(right)) =>
      if (depth >= 4) JustExploded(SnailFishValue(0), left, right)
      else NotExploded(number)
    case SnailFishPair(left, right) => explode(left, depth + 1) match {
      case Exploded(newValue) => Exploded(SnailFishPair(newValue, right))
      case JustExploded(newValue, leftAdd, rightAdd) => LeftExploded(SnailFishPair(newValue, addLeft(right, rightAdd)), leftAdd)
      case LeftExploded(value, add) => LeftExploded(SnailFishPair(value, right), add)
      case RightExploded(value, add) => Exploded(SnailFishPair(value, addLeft(right, add)))
      case NotExploded(leftValue) => explode(right, depth + 1) match {
        case NotExploded(value) => NotExploded(SnailFishPair(leftValue, value))
        case Exploded(value)    => Exploded(SnailFishPair(leftValue, value))
        case JustExploded(value, leftAdd, rightAdd) => RightExploded(SnailFishPair(addRight(leftValue, leftAdd), value), rightAdd)
        case LeftExploded(value, add) => Exploded(SnailFishPair(addRight(leftValue, add), value))
        case RightExploded(value, add) => RightExploded(SnailFishPair(leftValue, value), add)
      }
    }
    case SnailFishValue(_) => NotExploded(number)
  }

  def split(number: SnailFishNumber): SplitResult = number match {
    case SnailFishPair(left, right) =>
      split(left) match {
        case Split(value) => Split(SnailFishPair(value, right))
        case NotSplit(leftValue) => split(right) match {
            case Split(value)    => Split(SnailFishPair(leftValue, value))
            case NotSplit(value) => NotSplit(SnailFishPair(leftValue, value))
          }
      }
    case SnailFishValue(value) if value >= 10 => Split(SnailFishPair(SnailFishValue(Math.floor(value.toDouble / 2).toInt), SnailFishValue(Math.ceil(value.toDouble / 2).toInt)))
    case SnailFishValue(_) => NotSplit(number)
  }

  def addLeft(number: SnailFishNumber, value: Int): SnailFishNumber =
    if (value == 0) number
    else {
      number match {
        case SnailFishPair(left, right) => SnailFishPair(addLeft(left, value), right)
        case SnailFishValue(number) => SnailFishValue(number + value)
      }
    }

  def addRight(number: SnailFishNumber, value: Int): SnailFishNumber =
    if (value == 0) number
    else {
      number match {
        case SnailFishPair(left, right) => SnailFishPair(left, addRight(right, value))
        case SnailFishValue(number)        => SnailFishValue(number + value)
      }
    }

  def calculateMagnitude(number: SnailFishNumber): BigInt = number match {
    case SnailFishValue(number) => number
    case SnailFishPair(left, right) => calculateMagnitude(left) * 3 + calculateMagnitude(right) * 2
  }
}
