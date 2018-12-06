package aoc2018

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by 
  *
  * @author Raphael Ludwig
  * @version 05.12.18
  */
object Day5 extends App {

  val in: String = Source.fromResource("day5.txt").mkString

  println(s" * Day 5: ")
  println(s"   - Part 1: ${react(in).length}")
  println(s"   - Part 2: ${in.toLowerCase.distinct.par.map { remove =>
    react(in.filterNot(c => c == remove || c == remove.toUpper)).length
  }.min}")


  def react(in: String): String = {
    val out = new ArrayBuffer[Char]

    in.foreach {
      case a if out.isEmpty                               => out.append(a)
      case c if out.last.isUpper && c != out.last.toLower => out.append(c)
      case c if out.last.isLower && c != out.last.toUpper => out.append(c)
      case _ => out.trimEnd(1)
    }

    out.mkString
  }

}
