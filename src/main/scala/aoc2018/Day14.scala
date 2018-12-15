package aoc2018

import scala.collection.mutable
import scala.io.Source

/**
  * Created by 
  *
  * @author Raphael Ludwig
  * @version 14.12.18
  */
object Day14 extends App {

  implicit class RichInteger(val int: Int) extends AnyVal {
    def toDigits: IndexedSeq[Int] = if (int > 0) (int / 10).toDigits ++ Vector(int % 10) else Vector.empty
  }

  val in = Source.fromResource("day14.txt").getLines().next().toInt
  val elfs = Array(0,1)

  def printScores(elfs: Array[Int], scoreBoard: mutable.Buffer[Int]): Unit = {
    println(
      scoreBoard.indices.map {
        case x if x == elfs(0) => s"(${scoreBoard(x)})"
        case x if x == elfs(1) => s"[${scoreBoard(x)}]"
        case x => s" ${scoreBoard(x)} "
      }.mkString
    )
  }

  def cook(elfs: Array[Int], scoreBoard: mutable.Buffer[Int]): Unit = {
    val n = scoreBoard(elfs(0)) + scoreBoard(elfs(1))

    if (n >= 10) {
      scoreBoard += 1
      scoreBoard += n - 10
    } else {
      scoreBoard += n
    }

    elfs(0) = (elfs(0) + scoreBoard(elfs(0)) + 1) % scoreBoard.length
    elfs(1) = (elfs(1) + scoreBoard(elfs(1)) + 1) % scoreBoard.length
  }

  def part1(after: Int, scoreBoard: mutable.Buffer[Int]): String = scoreBoard.slice(after, after + 10).mkString

  def solveForScore(after: Int, score: (Int, mutable.Buffer[Int]) => String): String = {
    val scores = Array(3,7).toBuffer
    val elfs = Array(0,1)

    Stream.from(1).takeWhile(_ => scores.length < after + 10).foreach { _ => cook(elfs, scores) }
    score(after, scores)
  }

  def solvePart2(target: String): Int = {
    val scores = Array(3,7).toBuffer
    val elfs = Array(0,1)
    val t = target.map(_.toInt - '0'.toInt)

    def reverseContains: Boolean = {
      val itr = scores.reverseIterator
      val tit = t.reverseIterator
      var same = false

      do {
        same = itr.next() == tit.next()
      } while(same && itr.hasNext && tit.hasNext)

      same && !tit.hasNext
    }

    Stream.from(1).takeWhile(_ => !reverseContains).foreach { _ =>
      val n = scores(elfs(0)) + scores(elfs(1))

      if (n >= 10) {
        scores += 1
        if (reverseContains)
          return scores.length - t.length

        scores += n - 10
      } else {
        scores += n
      }

      elfs(0) = (elfs(0) + scores(elfs(0)) + 1) % scores.length
      elfs(1) = (elfs(1) + scores(elfs(1)) + 1) % scores.length
    }

    scores.length - t.length
  }


  println(s"   - Part 1: ${solveForScore(in, part1)}")
  println(s"   - Part 2: ${solvePart2(in.toString)}")

}
