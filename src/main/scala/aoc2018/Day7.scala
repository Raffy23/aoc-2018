package aoc2018


import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

/**
  * Created by 
  *
  * @author Raphael Ludwig
  * @version 07.12.18
  */
object Day7 extends App {

  case class Node(name: Char, parents: ArrayBuffer[Node], children: mutable.TreeSet[Char]) {
    def canComplete: Boolean = children.isEmpty
    def tick(): Int = { time = time - 1; time }
    var time: Int = 60 + (name.toInt - 'A'.toInt) + 1
  }
  object Node {
    def apply(name: String): Node = new Node(name.head, new ArrayBuffer[Node](), new mutable.TreeSet())
  }

  val Pattern = "Step ([A-Z]) must be finished before step ([A-Z]) can begin.".r

  def buildNodeTree(): mutable.TreeMap[Char, Node] = {
    val nodes = new mutable.TreeMap[Char, Node]()

    Source.fromResource("day7.txt").getLines().toStream.foreach {
      case Pattern(child, name) =>
        val childNode = nodes.getOrElseUpdate(child.head, Node(child))
        val node      = nodes.getOrElseUpdate(name.head, Node(name))
        childNode.parents.append(node)
        nodes(name.head).children.add(child.head)
    }

    nodes
  }


  val part1 = new ListBuffer[Char]
  val part1_nodes = buildNodeTree()

  while(part1_nodes.nonEmpty) {
    part1_nodes
      .filter{ case (_, node) => node.canComplete }
      .toList
      .sortBy(_._1.toInt)
      .headOption
      .foreach { case(name, node) =>
          part1_nodes.remove(name)
          node.parents.foreach(_.children.remove(name))

          part1.prepend(name)
      }
  }

  println(s"   - Part 1: ${part1.mkString.reverse}")

  val part2_nodes = buildNodeTree()
  val worker = Array.fill[Option[Node]](5)(None)
  var finishedConstruction = false

  val time = Stream.from(0).takeWhile(_ => !finishedConstruction).map{ time =>
    part2_nodes
      .filter{ case (_, node) => node.canComplete }
      .toList
      .sortBy(_._1.toInt)
      .take(worker.count(_.isEmpty) + 1)
      .filter(target => !worker.map(_.map(_.name).getOrElse('.')).contains(target._1))
      .foreach{ case (_, node) =>
          worker(worker.indexWhere(_.isEmpty)) = Some(node)
      }

    worker.zipWithIndex.foreach{ case (nodeOpt, idx) =>
      if(nodeOpt.isDefined && nodeOpt.get.tick() == 0) {
        val node = nodeOpt.get

        node.parents.foreach(_.children.remove(node.name))
        part2_nodes.remove(node.name)

        worker(idx) = None
      }
    }

    finishedConstruction = part2_nodes.isEmpty && worker.count(_.nonEmpty) == 0
    time
  }.last + 1

  println(s"   - Part 2: $time")

}
