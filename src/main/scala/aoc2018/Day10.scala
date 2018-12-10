package aoc2018

import scala.io.Source

/**
  * Created by 
  *
  * @author Raphael Ludwig
  * @version 10.12.18
  */
object Day10 extends App {

  case class Light(var x: Int, var y: Int, velocity: (Int,Int)) {
    def isAt(light: Light): Boolean = light.x == this.x && light.y == this.y
    def isAt(x: Int, y: Int): Boolean = x == this.x && y == this.y
    def move(): Unit = {
      this.x = x + velocity._1
      this.y = y + velocity._2
    }
  }

  val Pattern = "position=<[\\s]*([0-9-]+),[\\s]*([0-9-]+)> velocity=<[\\s]*([0-9-]+),[\\s]*([0-9-]+)>".r

  val in = Source.fromResource("day10.txt").getLines().toList.map{
    case Pattern(pX,pY,vX,vY) => Light(pX.toInt,pY.toInt,(vX.toInt,vY.toInt))
  }

  def isWord(lights: List[Light]): Boolean = {
    var hasNeighbours = true

    lights.takeWhile(_ => hasNeighbours).foldLeft(true){
      case (false, _) => hasNeighbours = false; false
      case (true, light) => lights.exists {
        case other if other.isAt(light.x - 1, light.y - 1) => true
        case other if other.isAt(light.x - 1, light.y    ) => true
        case other if other.isAt(light.x    , light.y - 1) => true
        case other if other.isAt(light.x + 1, light.y    ) => true
        case other if other.isAt(light.x    , light.y + 1) => true
        case other if other.isAt(light.x + 1, light.y + 1) => true
        case other if other.isAt(light.x + 1, light.y - 1) => true
        case other if other.isAt(light.x - 1, light.y + 1) => true
        case _ => false
      }
    }
  }

  val time = Stream.from(0).takeWhile(_ => !isWord(in)).map{ x => in.foreach(_.move()); x }.last + 1

  var minX = in.map(_.x).min
  var maxX = in.map(_.x).max
  var minY = in.map(_.y).min
  var maxY = in.map(_.y).max

  println("   - Part 1:")
  (minY to maxY).foreach { y =>
    print("             ")

    (minX to maxX).foreach { x =>
      print(in.find(_.isAt(x,y)).map(_ => '#').getOrElse(" "))
    }
    println()
  }

  println(s"   - Part 2: $time")

}
