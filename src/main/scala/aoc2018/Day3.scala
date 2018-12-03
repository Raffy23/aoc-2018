package aoc2018

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
  * Created by 
  *
  * @author Raphael
  * @version 03.12.2018
  */
object Day3 extends App {

  case class Claim(id: String, left: Int, top: Int, width: Int, height: Int, var collision: Boolean = false)
  val ClaimPattern = "^#([a-zA-Z0-9]+) @ (\\d+),(\\d+): (\\d+)x(\\d+)$".r

  val in = Source.fromResource("day3.txt").getLines().toStream.map {
    case ClaimPattern(id, left, top, width, height) => Claim(id, left.toInt, top.toInt, width.toInt, height.toInt)
  }


  val max_width = in.map(claim => claim.left + claim.width).max
  val max_height = in.map(claim => claim.top + claim.height).max
  val fabric: Array[Array[ListBuffer[Claim]]] = Array.fill(max_width + 1, max_height + 1)(new ListBuffer())

  in.foreach{ claim =>
    (0 until claim.width).foreach{ column =>
      (0 until claim.height).foreach{ row =>
        val fabricSpot = fabric(claim.left+column)(claim.top+row)

        if (fabricSpot.nonEmpty)
          claim.collision = true

        fabricSpot.foreach(_.collision = true)
        fabricSpot += claim
      }
    }
  }

  println("=== AoC - 2018 ===")
  println(" * Day3: ")
  println(s"   - Part1: ${fabric.map(_.count(_.size>=2)).sum}")
  println(s"   - Part2: ${in.filter(_.collision == false).map(_.id).toList.mkString(",")}")


}
