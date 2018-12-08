package aoc2018

import scala.io.Source

/**
  * Created by: 
  *
  * @author Raphael
  * @version 08.12.2018
  */
object Day8 extends App {

  class Node(children: IndexedSeq[Node], metadata: IndexedSeq[Int]) {
    def sumMetadata: Int = children.map(_.sumMetadata).sum + metadata.sum
    def value: Int =
      if (children.isEmpty) metadata.sum
      else metadata.map {
        case 0 => 0
        case idx if (idx-1) >= children.length => 0
        case idx => children(idx-1).value
      }.sum
  }

  def in = Source.fromResource("day8.txt").mkString.split(" ").map(_.toInt).toIterator

  def parseNode(in: Iterator[Int]): Node = {
    val children = in.next()
    val metadata = in.next()

    new Node(
      (0 until children).map(_ => parseNode(in)),
      (0 until metadata).map(_ => in.next())
    )
  }

  println(s"   - Part 1: ${parseNode(in).sumMetadata}")
  println(s"   - Part 2: ${parseNode(in).value}")

}
