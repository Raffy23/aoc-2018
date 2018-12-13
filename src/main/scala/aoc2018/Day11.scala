package aoc2018

import scala.io.Source

/**
  * Created by 
  *
  * @author Raphael Ludwig
  * @version 12.12.18
  */
object Day11 extends App {

  val gridSerialNumber = Source.fromResource("day11.txt").getLines().next().toInt
  val gridSize = 300

  case class PowerRegion(x: Int, y: Int, level: Int) {
    override def toString: String = s"PowerRegion(x=${x+1}, y=${y+1}, level=$level)"
  }
  val NoPower = PowerRegion(0,0,Integer.MIN_VALUE)

  def genPowerGrid(serial: Int, maxX: Int, maxY: Int): IndexedSeq[IndexedSeq[Int]] =
    (1 to maxX).map{ x =>
      (1 to maxY).map{ y =>
        val rackID = x + 10
        ((((rackID * y) + serial) * rackID) / 100) % 10 - 5
      }
    }

  def findBiggestCell(grid: IndexedSeq[IndexedSeq[Int]], cellSize: Int = 2): PowerRegion =
    grid.map(_.zipWithIndex).zipWithIndex.foldLeft(NoPower){ case (power, (column, x)) =>
      column.foldLeft(power){
        case (power, (_, y)) if x+cellSize >= grid.length || y+cellSize >= grid(x).length => power
        case (power, (_, y)) =>
          val powerLevel =
            (x to x+cellSize).map{ x =>
                (y to y+cellSize).map{ y =>
                  grid(x)(y)
                }.sum
            }.sum

          if (powerLevel > power.level) PowerRegion(x,y, powerLevel)
          else power
      }
  }


  val powerGrid = genPowerGrid(gridSerialNumber, gridSize, gridSize)

  println(s"   - Part 1: ${findBiggestCell(powerGrid)}")

  // Slow, don't run ...
  println(s"   - Part 2: ${(1 to gridSize).par.map(size => (size, findBiggestCell(powerGrid, size))).maxBy(_._2.level)}")

}
