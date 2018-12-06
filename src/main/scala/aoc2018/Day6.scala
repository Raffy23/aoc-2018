package aoc2018

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import scala.io.Source

/**
  * Created by 
  *
  * @author Raphael Ludwig
  * @version 06.12.18
  */
object Day6 extends App {

  case class Location(name: Char, x: Int, y: Int, size: AtomicInteger = new AtomicInteger(0), endless: AtomicBoolean = new AtomicBoolean(false)) {
    def distance(x: Int, y: Int): Int = Math.abs(x - this.x) + Math.abs(y - this.y)
  }
  val Pattern = "^(\\d+), (\\d+)$".r

  var sName = ('A' - 1).toChar
  val in = Source.fromResource("day6.txt").getLines().toStream.map {
    case Pattern(x,y) => sName = (sName + 1).toChar; Location(sName, x.toInt, y.toInt)
    case _ => throw new RuntimeException("Error: Input format must be: <number>, <number>")
  }

  val minX = in.map(_.x).min
  val minY = in.map(_.y).min
  val maxX = in.map(_.x).max //+ 1
  val maxY = in.map(_.y).max //+ 1

  (minY to maxY).par.foreach { y =>
    (minX to maxX).foreach { x =>

      val distances = in.map(point => (point, point.distance(x,y))).groupBy(_._2)
      val closestDistance = distances.keys.min

      if (distances(closestDistance).length == 1) {
        val point = distances(closestDistance).head._1
        point.size.incrementAndGet()

        if (y == 0 || x == 0 || y == maxY || x == maxX)
          point.endless.set(true)

      }
    }
  }

  println(s" * Day 6: ")
  println(s"   - Part 1: ${in.filter(_.endless.get == false).maxBy(_.size.get).size.get}")

  val threshold = 10000
  var size = 0

  (minY to maxY).foreach { y =>
    (minX to maxX).foreach { x =>

      if(in.map(point => point.distance(x,y)).sum < threshold) {
        size += 1
      }
    }
  }

  println(s"   - Part 2: $size")

}
