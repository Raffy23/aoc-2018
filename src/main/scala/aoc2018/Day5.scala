package aoc2018

import java.util

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
  * Created by 
  *
  * @author Raphael Ludwig
  * @version 05.12.18
  */
object Day5 extends App {

  val in: String = Source.fromResource("day5.txt").mkString

  println(s" * Day 4: ")
  println(s"   - Part 1: ${react(in).length}")
  println(s"   - Part 2: ${in.toLowerCase.distinct.map { remove =>
    react(in.replaceAll(s"$remove|${remove.toUpper}", "")).length
  }.min}")


  def react(in: String): String = {
    val out = new ListBuffer[Char]

    in.foreach {
      case a if out.isEmpty                               => out.append(a)
      case c if out.last.isUpper && c != out.last.toLower => out.append(c)
      case c if out.last.isLower && c != out.last.toUpper => out.append(c)
      case _ => out.trimEnd(1)
    }

    out.mkString
  }

}
