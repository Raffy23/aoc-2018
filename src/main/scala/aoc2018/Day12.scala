package aoc2018

import java.util

import scala.collection.parallel.ParSeq
import scala.io.Source

/**
  * Created by 
  *
  * @author Raphael Ludwig
  * @version 12.12.18
  */
object Day12 extends App {

  type Plant = Boolean
  val Pattern = "([\\.#]{5}) => ([\\.#])".r

  implicit class PotConverter(val in: String) extends AnyVal {
    def toPots: Array[Plant] = in.toCharArray.map{
      case '#' => true
      case '.' => false
    }
  }

  val in = Source.fromResource("day12.txt").getLines()
  var pots = in.next().split(": ")(1).toPots

  // Empty file
  in.next()

  case class Rule(pattern: Array[Plant], outcome: Boolean)
  val rules = in.toStream
    .map{
      case Pattern(in, "#") => Rule(in.toPots, outcome = true)
      case Pattern(_, ".")  => Rule(Array.ofDim(0), outcome = false)
    }
    .filter(_.outcome)
    .map{ rule => hashWindow(rule.pattern,0) }
    .toSet

  def hashWindow(array: Array[Plant], start: Int, size: Int = 5, empty: Boolean = false): Long = {
    var out = 0L

    for(x <- 0 until size) {
      val value = if ((start + x) < 0 || (start + x) >= array.length) false else array(start + x)
      if (value)
        out += 1

      out <<= 1
    }

    out
  }

  def applyRules(pots: Array[Plant]): Seq[Int] =
    (-5 to pots.length + 4).map {
      case i if rules.contains(hashWindow(pots, i)) => Some(i + 2)
      case _ => None
    }.filter(_.isDefined)
      .map(_.get)


  def simulate(pots_in: Array[Plant], sec: Long): Long = {

    var negativeGrowth = 0
    var pots = pots_in
    var i = sec

    while(i > 0) {
      val newPots = applyRules(pots)

      val min = newPots.min
      val max = newPots.max

      val prepend = if (min < 0) Math.abs(min) else 0
      val append  = if (max+prepend+1 >= pots.length) (max+prepend+1) - pots.length else 0

      if (min < 0)
        negativeGrowth += min

      if (append != 0 || prepend != 0) {
        pots = Array.fill[Plant](append + prepend + pots.length)(false)
      } else {
        util.Arrays.fill(pots, false)
      }

      newPots.foreach(idx => pots(idx+prepend) = true)
      i -= 1

      if (i % 10000L == 0) println(s"           ${50000000000L-i} / 50000000000 | ${pots.zipWithIndex.filter(_._1).map(_._2 + negativeGrowth).sum}")
    }

    pots.zipWithIndex.filter(_._1).map(_._2 + negativeGrowth).sum
  }


  println(s"   - Part 1: ${simulate(pots, 20)}")

  // Use the output of Part 2 and calculate the stuff ...
  println(s"            (look at the values to solve the challenge)")
  println(s"   - Part 2: ${simulate(pots, 50000000000L)}")



}
