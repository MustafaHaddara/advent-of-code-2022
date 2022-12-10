package solutions

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable
import solutions.Day

class Day09 extends Day:
  val day = 1;
  val testInput = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"""

  class Point(val x: Int, val y: Int):
    def inc(dir: String): Point =
      dir match
        case "U" => return Point(x, y + 1)
        case "D" => return Point(x, y - 1)
        case "L" => return Point(x - 1, y)
        case "R" => return Point(x + 1, y)

    override def toString: String =
      s"($x, $y)"

  def solveA(input: List[String]): String =
    var head = Point(0, 0)
    var tail = Point(0, 0)
    var seen = mutable.Set[Tuple]()

    for (line <- input) {
      val pair = line.split(" ")
      val dir = pair(0)
      val amt = pair(1).toInt
      for (i <- 0 until amt) {
        head = head.inc(dir)

        if (!isAdjacent(head, tail)) {
          tail = Point(stepOne(tail.x, head.x), stepOne(tail.y, head.y))
        }
        seen.add((tail.x, tail.y))
      }
    }
    return seen.size.toString()

  def stepOne(current: Int, target: Int): Int =
    if (target > current) {
      return current + 1
    } else if (target < current) {
      return current - 1
    }
    return current

  def isAdjacent(a: Point, b: Point): Boolean =
    val xadj =
      (b.x - 1 <= a.x && a.x <= b.x + 1) || (a.x - 1 <= b.x && b.x <= a.x + 1)
    val yadj =
      (b.y - 1 <= a.y && a.y <= b.y + 1) || (a.y - 1 <= b.y && b.y <= a.y + 1)
    return xadj && yadj

  def solveB(input: List[String]): String =
    var points = mutable.ArraySeq.fill(10) { Point(0, 0) }

    var seen = mutable.Set[Tuple]()

    for (line <- input) {
      val pair = line.split(" ")
      val dir = pair(0)
      val amt = pair(1).toInt
      for (i <- 0 until amt) {
        points(0) = points(0).inc(dir)

        for (idx <- 1 to 9) {
          val head = points(idx - 1)
          var tail = points(idx)
          if (!isAdjacent(head, tail)) {
            tail = Point(stepOne(tail.x, head.x), stepOne(tail.y, head.y))
            points(idx) = tail
          }
        }

        val tail = points(9)
        seen.add((tail.x, tail.y))
      }
    }
    return seen.size.toString()
