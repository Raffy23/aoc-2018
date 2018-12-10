package aoc2018

/**
  * Created by 
  *
  * @author Raphael Ludwig
  * @version 04.12.18
  */
object AoCRunner extends App {

  def resource(day: Int): String = s"/day$day.txt"

  println("=== AoC - 2018 ===")
  List(
    Day1,Day2,Day3,Day4,Day5,Day6,Day7,
    Day8,Day9,Day10
  ).zipWithIndex.foreach{
    case (app, day) => runIfExists(day+1, app)
  }

  def runIfExists(day: Int, app: App): Unit = {
    if(AoCRunner.getClass.getResource(resource(day)) != null) {

      println(s" * Day $day: ")

      // Time measurement should be without reading input data, but
      // this is good enough
      val timer = System.currentTimeMillis()
      app.main(args)

      println(s"     (Time needed: ${System.currentTimeMillis() - timer} ms)")
    }
    else println(s" * Day $day: (skipping no resource found)")

  }

}
