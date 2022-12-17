package solutions

import scala.collection.mutable

import solutions.Day

class Day14 extends Day:
  val day = 14;
  val testInput = """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"""

  class Point(val x: Int, val y: Int) extends Ordered[Point]:
    def this(arr: Array[Int]) =
      this(arr(0), arr(1))
    def this(str: String) =
      this(str.split(",").map(c => c.toInt))

    override def toString(): String =
      return s"<$x,$y>"
    override def equals(other: Any): Boolean =
      if (other.isInstanceOf[Point]) {
        val p2 = other.asInstanceOf[Point]
        return p2.x == x && p2.y == y
      }
      return false
    override def hashCode(): Int =
      return (x, y).hashCode()
    override def compare(other: Point) =
      val c = x compare other.x
      if (c == 0) {
        return y compare other.y
      }
      return c

  def stepTo(start: Int, end: Int): Int =
    start - (start compare end)

  def stepTo(start: Point, end: Point): Point =
    return Point(stepTo(start.x, end.x), stepTo(start.y, end.y))

  def parseInput(input: List[String]): mutable.Map[Point, Char] =
    val map = mutable.Map[Point, Char]()
    for (line <- input) {
      val chunks = line.split(" -> ")
      for (i <- 0 until chunks.length - 1) {
        var start = Point(chunks(i))
        val end = Point(chunks(i + 1))

        map.put(start, '#')
        while (start != end) {
          start = stepTo(start, end)
          map.put(start, '#')
        }
      }
    }

    return map

  def nextLocation(current: Point, occupied: mutable.Map[Point, Char]): Point =
    val below = Point(current.x, current.y + 1)
    if (!occupied.contains(below)) {
      return below
    }

    val belowLeft = Point(current.x - 1, current.y + 1)
    if (!occupied.contains(belowLeft)) {
      return belowLeft
    }

    val belowRight = Point(current.x + 1, current.y + 1)
    if (!occupied.contains(belowRight)) {
      return belowRight
    }

    // out of options
    return current

  def solveA(input: List[String]): String =
    val map = parseInput(input)

    val deepestPoint = map.keys.toArray.map(p => p.y).max

    val startingPoint = Point(500, 0)
    var sands = 0
    var s = startingPoint
    while (true) {
      val next = nextLocation(s, map)
      if (next == s) {
        map.put(s, 'O')
        sands += 1
        s = startingPoint
      } else if (next.y > deepestPoint) {
        // impossible to stop
        return sands.toString()
      } else {
        s = next
      }
    }

    return "A"

  def solveB(input: List[String]): String =
    val map = parseInput(input)

    val deepestPoint = map.keys.toArray.map(p => p.y).max
    val leftMost = map.keys.toArray.map(p => p.x).min
    val rightMost = map.keys.toArray.map(p => p.x).max

    val startingPoint = Point(500, 0)
    var sands = 0
    var s = startingPoint
    while (true) {
      val next = nextLocation(s, map)
      if (next == startingPoint) {
        // all done
        return (sands + 1).toString()
      }
      if (next.y == deepestPoint + 1) {
        // cheeky way to get into the next if statement with the right vals
        s = next
      }
      if (next == s) {
        map.put(s, 'O')
        sands += 1
        s = startingPoint
      } else {
        s = next
      }
    }

    return "B"
