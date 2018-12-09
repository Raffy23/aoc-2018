package aoc2018

import scala.io.Source

/**
  * Created by: 
  *
  * @author Raphael
  * @version 09.12.2018
  */
object Day9 extends App {

  val Pattern = "(\\d+) players; last marble is worth (\\d+) points".r

  val in = Source.fromResource("day9.txt").getLines().toStream.map{
    case Pattern(players, points) => (players.toInt, points.toInt)
  }

  val input = in.head

  def calculateHighestScore(players: Int, maxMarble: Int): Long = {
    val scores = Array.fill[Long](players)(0L)
    val marbles = new CircularDoubleLinkedList[Int](0)

    Stream
      .continually(1 to players)
      .flatten
      .zipWithIndex
      .take(maxMarble)
      .map(x => (x._1, x._2 + 1))
      .foreach{
        case (player, marble) if marble % 23 == 0 =>
          scores(player-1) += marble + marbles.prev(7)
          marbles.remove()

        case (_, marble) =>
          marbles.insertIn(marble, 2)
      }

    scores.max
  }

  println(s"   - Part 1: ${calculateHighestScore(input._1, input._2)}")
  println(s"   - Part 2: ${calculateHighestScore(input._1, input._2 * 100)}")


  class CircularDoubleLinkedList[T](root: T) {
    private class Node(val value: T, var next: Node, var prev: Node) {
      override def toString: String = value.toString
    }
    private object Node {
      def apply(value: T): Node = {
        val a = new Node(value, null, null)
        a.next = a
        a.prev = a
        a
      }
    }

    private var headNode = Node(root)
    private var elements = 1

    @inline def head: T = headNode.value

    @inline def next(): T = { headNode = headNode.next; headNode.value }
    @inline def prev(): T = { headNode = headNode.prev; headNode.value }

    @inline def next(steps: Int): T = { (0 until steps).foreach(_ => headNode = headNode.next); headNode.value }
    @inline def prev(steps: Int): T = { (0 until steps).foreach(_ => headNode = headNode.prev); headNode.value }

    @inline def insert(value: T): Unit = {
      val n = new Node(value, headNode, headNode.prev)
      headNode.prev.next = n
      headNode.prev = n
      headNode = n
      elements += 1
    }

    @inline def insertIn(value: T, steps: Int): Unit = {
      next(steps)
      insert(value)
    }

    @inline def remove(): Unit = {
      headNode.next.prev = headNode.prev
      headNode.prev.next = headNode.next

      headNode = headNode.next
      elements -= 1
    }

    @inline def size(): Int = elements

    override def toString: String = s"CircularDoubleLinkedList(size=$elements, elements=${
      (1 to elements).map{i =>
        val value = headNode.value
        headNode = headNode.next

        value
      }.mkString(", ")
    })"
  }


}
