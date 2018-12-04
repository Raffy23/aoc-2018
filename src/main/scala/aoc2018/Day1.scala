package aoc2018

import scala.collection.mutable
import scala.io.Source

/**
  * Created by: 
  *
  * @author Raphael
  * @version 02.12.2018
  */
object Day1 extends App {

  lazy val in = Source.fromResource("day1.txt").getLines().toStream.map(_.toInt)
  lazy val endless: Stream[Int] = in #::: endless
  val foundFreq = new mutable.TreeSet[Int]()
  var foundDuplicate = false

  println(" * Day 1: ")
  println(s"   - Part 1: ${in.sum}")
  println(s"   - Part 2: ${endless.takeWhile(_ => !foundDuplicate).reduce{ (a,b) =>
    foundDuplicate = !foundFreq.add(a+b)
    a+b
  }}")

}
