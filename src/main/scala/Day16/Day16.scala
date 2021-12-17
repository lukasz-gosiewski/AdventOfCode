package com.gosiewski
package Day16

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day16 extends App {
  // Can we somehow rework this with use of 3 types - Packet, OperatorPacket and ValuePacket? So that not every packet has value? Ot at least this value is calculated differently taking the type into cosideration?
  case class Packet(version: Int, typeID: Int, value: BigInt, subPackets: Seq[Packet])

  val hexToBinMap = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )

  val hexInput = getInputHexString("input-16.txt")
  val packet = decodePacket(translateToBinString(hexInput))
  val versionSum = calculateVersionSum(packet._1)
  val value = calculateValue(packet._1)

  println(packet)
  println(s"Version sum for package is $versionSum")
  println(s"Value for package is $value")

  def getInputHexString(filename: String): String =
    Source.fromResource(filename)
      .getLines()
      .next()

  def translateToBinString(hexString: String): String =
    hexString.toCharArray
      .flatMap(hexCharacter => hexToBinMap(hexCharacter).toCharArray)
      .mkString

  def decodePacket(binString: String): (Packet, String) = {
    val version = Integer.parseInt(binString.take(3), 2)
    val typeID = Integer.parseInt(binString.slice(3, 6), 2)

    if (typeID == 4) {
      val valuePackets = binString.slice(6, binString.length).grouped(5)

      var binaryNumber = ""
      var currPacket = valuePackets.next()
      while (currPacket(0).equals('1')) {
        binaryNumber += currPacket.slice(1, currPacket.length)
        currPacket = valuePackets.next()
      }
      binaryNumber += currPacket.slice(1, currPacket.length)

      val rest = valuePackets.toSeq.flatten.mkString

      (Packet(version, typeID, BigInt(binaryNumber, 2), Seq.empty), rest)
    } else {
      val lengthTypeId = Integer.parseInt(binString(6).toString, 2)

      if (lengthTypeId == 0) {
        val totalBitsLength = Integer.parseInt(binString.slice(7, 22), 2)
        var toBeParsed = binString.slice(22, 22 + totalBitsLength)
        val rest = binString.slice(22 + totalBitsLength, binString.length)

        val subpackets: mutable.ListBuffer[Packet] = ListBuffer.empty
        while (toBeParsed.nonEmpty) {
          val result = decodePacket(toBeParsed)
          subpackets.addOne(result._1)
          toBeParsed = result._2
        }

        (Packet(version, typeID, BigInt(0), subpackets.toSeq), rest)
      } else {
        val numberOfPackets = Integer.parseInt(binString.slice(7, 18), 2)

        var toBeParsed = binString.slice(18, binString.length)
        val subpackets: mutable.ListBuffer[Packet] = ListBuffer.empty
        for (_ <- 0 until numberOfPackets) {
          val result = decodePacket(toBeParsed)
          subpackets.addOne(result._1)
          toBeParsed = result._2
        }

        (Packet(version, typeID, BigInt(0), subpackets.toSeq), toBeParsed)
      }
    }
  }

  private def calculateVersionSum(packet: Packet): BigInt = {
    if (packet.subPackets.isEmpty) packet.version
    else {
      var counter = BigInt(packet.version)
      for (subpacket <- packet.subPackets) counter += calculateVersionSum(subpacket)

      counter
    }
  }

  private def calculateValue(packet: Packet): BigInt = {
    packet.typeID match {
      case 0 =>
        if (packet.subPackets.length == 1) calculateValue(packet.subPackets.head)
        else {
          var sum = BigInt(0)
          for (subpacket <- packet.subPackets) sum += calculateValue(subpacket)

          sum
        }
      case 1 =>
        if (packet.subPackets.length == 1) calculateValue(packet.subPackets.head)
        else {
          var product = BigInt(1)
          for (subpacket <- packet.subPackets) product *= calculateValue(subpacket)

          product
        }
      case 2 => calculateValue(packet.subPackets.minBy(packet => calculateValue(packet)))
      case 3 => calculateValue(packet.subPackets.maxBy(packet => calculateValue(packet)))
      case 4 => packet.value
      case 5 => if (calculateValue(packet.subPackets(0)) > calculateValue(packet.subPackets(1))) 1 else 0
      case 6 => if (calculateValue(packet.subPackets(0)) < calculateValue(packet.subPackets(1))) 1 else 0
      case 7 => if (calculateValue(packet.subPackets(0)) == calculateValue(packet.subPackets(1))) 1 else 0
      case _ => throw new IllegalArgumentException("This operation type is not supported")
    }
  }
}
