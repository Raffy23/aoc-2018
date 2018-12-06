package aoc2018

import scala.io.Source

/**
  * Created by: 
  *
  * @author Raphael
  * @version 02.12.2018
  */
object Day2 extends App {

  val in = Source.fromResource("day2.txt").getLines().toStream

  println(" * Day 2: ")

  var twice = 0
  var threes = 0

  in.foreach(_.groupBy(identity).mapValues(_.length).values.toSet[Int].foreach{
    case 2 => twice += 1
    case 3 => threes += 1
    case _ =>
  })

  val minDiff = in.dropRight(1).zipWithIndex.map { case (str, idx) =>
    val v = in.drop(idx).map(other => (other, str.hammingDiff(other))).filter(_._2 > 0).minBy(_._2)
    (str, v._1, v._2)
  }.minBy(_._3)

  println(s"   - Part 1: $twice * $threes = ${twice * threes}")
  println(s"   - Part 2: ${minDiff._1 intersect minDiff._2}")

  implicit class HammingString(val str: String) extends AnyVal {
    def hammingDiff(other: String): Int = {
      str.indices.map(idx => other(idx) == str(idx)).count(!_)
    }
  }

}
