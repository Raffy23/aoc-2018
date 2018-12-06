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
  runIfExists(1, Day1)
  runIfExists(2, Day2)
  runIfExists(3, Day3)
  runIfExists(4, Day4)
  runIfExists(5, Day5)
  runIfExists(6, Day6)

  def runIfExists(day: Int, app: App): Unit = {
    if(AoCRunner.getClass.getResource(resource(day)) != null) {

      // Time measurement should be without reading input data, but
      // this is good enough
      val timer = System.currentTimeMillis()
      app.main(args)

      println(s"     (Time needed: ${System.currentTimeMillis() - timer} ms)")
    }
    else println(s" * Day $day: (skipping no resource found)")

  }

}
