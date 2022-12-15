package solutions

import scala.collection.mutable
import scala.math.floorDiv
import solutions.Day

class Day12 extends Day:
  val day = 10;
  val testInput = """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"""

  class Point(val x: Int, val y: Int):
    var partBCandidate = false
    var isStarted = false

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

    def neighbors(): List[Point] =
      return List(
        Point(x - 1, y),
        Point(x + 1, y),
        Point(x, y - 1),
        Point(x, y + 1),
      )

  def height(c: Char): Int =
    var height = c.toInt
    if (c == 'S') {
      height = 'a'.toInt
    }
    if (c == 'E') {
      height = 'z'.toInt
    }
    return height

  def buildAdjMap(
      input: List[String],
      start: Point,
      end: Point,
  ): Map[Point, mutable.ArrayBuffer[Point]] =
    val pts = mutable.Map[Tuple, Point](
      (start.x, start.y) -> start,
      (end.x, end.y) -> end,
    )
    val adj = mutable.Map[Point, mutable.ArrayBuffer[Point]]()

    for (y <- 0 until input.length) {
      val row = input(y)
      for (x <- 0 until row.length()) {
        val c = row(x)
        val p = pts.getOrElseUpdate((x, y), Point(x, y))
        if (c == 'S' || c == 'a') {
          p.partBCandidate = true
        }
        for (n1 <- p.neighbors()) {
          val n = pts.getOrElseUpdate((n1.x, n1.y), n1)
          if (
            0 <= n.x && n.x < row.length() && 0 <= n.y && n.y < input.length
          ) {
            val currentHeight = height(c)
            val otherHeight = height(input(n.y)(n.x))
            if (otherHeight <= currentHeight + 1) {
              adj.getOrElseUpdate(p, mutable.ArrayBuffer[Point]()) += n
            }
          }
        }
      }
    }

    return adj.toMap

  def getEndpoints(input: List[String]): (Point, Point) =
    var start = Point(0, 0)
    var end = Point(0, 0)
    for (y <- 0 until input.length) {
      val row = input(y)
      for (x <- 0 until row.length()) {
        val c = row(x)
        if (c == 'S') {
          start = Point(x, y)
        }
        if (c == 'E') {
          end = Point(x, y)
        }
      }
    }
    return (start, end)

  def dfsCount(
      start: Point,
      end: Point,
      adj: Map[Point, mutable.ArrayBuffer[Point]],
      currCount: Int,
      steps: mutable.Map[Point, Int],
  ): Unit =
    if (start.equals(end)) {
      return
    }

    var candidates = adj.getOrElse(start, mutable.ArrayBuffer[Point]())
    for (c <- candidates) {
      if (!c.isStarted) {
        c.isStarted = true
        val existing = steps.getOrElse(c, -1)
        if (existing == -1 || currCount < existing) {
          steps.put(c, currCount)
          dfsCount(c, end, adj, currCount + 1, steps)
        }
        c.isStarted = false
      }
    }

  def solveA(input: List[String]): String =
    val (start: Point, end: Point) = getEndpoints(input)
    val adjacencies = buildAdjMap(input, start, end)

    val steps = mutable.Map[Point, Int]()
    dfsCount(
      start,
      end,
      adjacencies,
      1,
      steps,
    )

    return steps.get(end).get.toString()

  def solveB(input: List[String]): String =
    val (start: Point, end: Point) = getEndpoints(input)
    val adjacencies = buildAdjMap(input, start, end)

    val candidates = adjacencies.keySet.filter(c => c.partBCandidate)

    val steps = mutable.Map[Point, Int]()
    var min = -1

    for (c <- candidates) {
      steps.clear()
      dfsCount(
        c,
        end,
        adjacencies,
        1,
        steps,
      )

      val distance = steps.get(end).getOrElse(-1)

      if (distance != -1 && (min == -1 || distance < min)) {
        min = distance
      }
    }

    return min.toString()
