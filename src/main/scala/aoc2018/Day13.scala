package aoc2018


import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by 
  *
  * @author Raphael Ludwig
  * @version 13.12.18
  */
object Day13 extends App {
  object Direction extends Enumeration {
    type Direction = Value

    val Left = Value('<')
    val Right = Value('>')
    val Up = Value('^')
    val Down = Value('v')

  }
  import Direction._

  type CartTrack = Array[Array[Char]]
  type Result = (ArrayBuffer[Cart], CartTrack)

  def track: (CartTrack, ArrayBuffer[Cart]) = {
    val carts = new ArrayBuffer[Cart]()
    val track = Source.fromResource("day13.txt").getLines().zipWithIndex.map{ case (line, x) =>
      line.zipWithIndex.map{
        case ('<', y) => carts += new Cart(x,y, Left);  '-'
        case ('>', y) => carts += new Cart(x,y, Right); '-'
        case ('^', y) => carts += new Cart(x,y, Up);    '|'
        case ('v', y) => carts += new Cart(x,y, Down);  '|'
        case (track, _) => track
      }.toArray
    }.toArray

    (track, carts)
  }

  def compute(part: Int, track: CartTrack, carts: ArrayBuffer[Cart]): Result = {

    var collision = false
    while (carts.size > 1 && !collision) {
      val rm = new ArrayBuffer[Cart]()

      carts.sortBy(c => (c.y, c.x)).foreach{cart =>
        if (!rm.contains(cart)) {
          cart.move(track)

          collision = carts.map(me => {
            val b = carts.filter(other => other != me && other.x == me.x && other.y == me.y)
            if (b.nonEmpty) {
              if (part == 1)
                track(me.x)(me.y) = 'X'

              rm += me
              b.foreach(rm += _)
            }

            b.nonEmpty
          }).exists(identity)
        }
      }

      carts --= rm
      rm.clear()

      if (part == 2)
        collision = false
    }

    (carts, track)
  }
  def solve(p: Int, in: (CartTrack, ArrayBuffer[Cart]) = track): Result = compute(p, in._1, in._2)

  def findCollision(track: CartTrack): String =
    track
      .zipWithIndex
      .map{ case (row, idx) => (row.indexWhere(_ == 'X'), idx)}
      .filter(_._1 > -1)
      .map(_.toString()).mkString(" ")

  println(s"   - Part 1: ${findCollision(solve(1)._2)}")
  println(s"   - Part 2: ${solve(2)._1.map(c => (c.y, c.x)).mkString}")


  class Cart(var x: Int, var y: Int, var direction: Direction) {
    private var turnCounter = -1L

    private def turn(): Option[Direction] = {
      turnCounter += 1

      turnCounter % 3 match {
        case 0 => Some(Left)
        case 1 => None
        case 2 => Some(Right)
      }
    }

    private def absTurn(direction: Direction): Direction = this.direction match {
      case Up => direction
      case Down if direction == Left => Right
      case Down if direction == Right => Left
      case Down => direction
      case Left if direction == Right => Up
      case Left if direction == Left => Down
      case Left => direction
      case Right if direction == Left => Up
      case Right if direction == Right => Down
      case Right => direction
    }

    private def advanceX(x: Int = this.x, direction: Direction = this.direction): Int =
      x + (if (direction == Up) -1 else if (direction == Down) 1 else 0)

    private def advanceY(y: Int = this.y, direction: Direction = this.direction): Int =
      y + (if (direction == Left) -1 else if (direction == Right) 1 else 0)

    def move(track: Array[Array[Char]]): Unit = {
      this.x = advanceX()
      this.y = advanceY()

      track(x)(y) match {
        case '+' => this.direction = turn().map(absTurn).getOrElse(this.direction)

        case '/' if direction == Left => this.direction = Down
        case '/' if direction == Up => this.direction = Right
        case '/' if direction == Right => this.direction = Up
        case '/' if direction == Down => this.direction = Left
        case '\\' if direction == Up => this.direction = Left
        case '\\' if direction == Right => this.direction = Down
        case '\\' if direction == Down => this.direction = Right
        case '\\' if direction == Left => this.direction = Up

        case ' ' => throw new RuntimeException("Cart outside of Track!")
        case _ =>
      }
    }
  }

}
