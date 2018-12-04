package aoc2018

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.collection.mutable
import scala.io.Source

/**
  * Created by 
  *
  * @author Raphael Ludwig
  * @version 04.12.18
  */
object Day4 extends App {

  sealed trait EntryType
  case class GuardEntry(id: Int)
  object FellAsleep
  object WokeUp

  implicit object LocalDateTimeOrdering extends Ordering[LocalDateTime] {
    override def compare(x: LocalDateTime, y: LocalDateTime): Int = x compareTo y
  }

  val fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
  val Pattern = """^\[([0-9-: ]+)\] (.*)""".r
  val IDPattern = "Guard #(\\d+) begins shift".r

  val in = Source.fromResource("day4.txt").getLines().toStream.map {
    case Pattern(date, entry) =>
      (LocalDateTime.parse(date, fmt),
        entry match {
          case "falls asleep" => FellAsleep
          case "wakes up" => WokeUp
          case IDPattern(id) => GuardEntry(id.toInt)
        }
      )
  }.sortBy(_._1)
    .map(data => (data._1.getDayOfYear, data._1.getMinute, data._2))

  println(" * Day 4: ")

  val sleepyGuards = new mutable.TreeMap[Int, Array[Int]]()

  var lastKnownGuard: Int  = 0
  var lastMinuteEntry: Int = 0
  in.foreach{
    case (_,      _, GuardEntry(id)) => lastKnownGuard = id
    case (_, minute, FellAsleep)     => lastMinuteEntry = minute
    case (_, minute, WokeUp)         =>
      val sleepyTime = sleepyGuards.getOrElseUpdate(lastKnownGuard, Array.fill(60)(0))
      (lastMinuteEntry until minute).foreach(idx => sleepyTime(idx) = sleepyTime(idx) + 1)

    // match should be exhaustive
    case _ => throw new RuntimeException("Ups, something went wrong, could not match value to any Event!")
  }

  val verySleepyGuard = sleepyGuards.maxBy(_._2.sum)
  val mostSleptMinute = verySleepyGuard._2.zipWithIndex.maxBy(_._1)._2

  println(s"   - Part1: ${verySleepyGuard._1} * $mostSleptMinute = ${verySleepyGuard._1 * mostSleptMinute}")

  val part2SleepyGuard = sleepyGuards.maxBy(_._2.max)
  val part2SleepyMinute = part2SleepyGuard._2.zipWithIndex.maxBy(_._1)._2

  println(s"   - Part2: ${part2SleepyGuard._1} * $part2SleepyMinute = ${part2SleepyGuard._1 * part2SleepyMinute}")
}
