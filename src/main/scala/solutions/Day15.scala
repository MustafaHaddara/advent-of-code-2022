package solutions

import scala.math.abs
import scala.collection.mutable

import solutions.Day

class Day15 extends Day:
  val day = 14;
  val testInput = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"""

  class Point(val x: Int, val y: Int) extends Ordered[Point]:
    def this(arr: Array[Int]) =
      this(arr(0), arr(1))
    def this(str: String) =
      // ex. x=9, y=16
      this(str.split(", ").map(s => s.split("=")(1).toInt))

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

  def slicePrefix(s: String, prefix: String): String =
    return s.slice(prefix.length(), s.length())

  def solveA(input: List[String]): String =
    // val targetY = 10 // 2000000 in the real input
    val targetY = 2000000 // in the real input

    val covered = mutable.Set[Int]()
    val beacons = mutable.Set[Int]()

    for (line <- input) {
      val chunks = line.split(": ")
      val sensorChunk = chunks(0)
      val beaconChunk = chunks(1)
      val sensor = Point(slicePrefix(sensorChunk, "Sensor at "))
      val beacon = Point(slicePrefix(beaconChunk, "closest beacon is at "))
      if (beacon.y == targetY) {
        beacons.add(beacon.x)
      }

      val dist = abs(sensor.x - beacon.x) + abs(sensor.y - beacon.y)
      val remaining = dist - abs(sensor.y - targetY)
      if (remaining > 0) {
        val min = sensor.x - remaining
        val max = sensor.x + remaining
        for (i <- min to max) {
          covered.add(i)
        }
      }
    }
    // println(covered.toArray.sorted.toList)

    return (covered.size - beacons.size).toString()

  def freq(point: Point): Long =
    return (point.x * 4000000L) + point.y

  def stepTo(start: Int, end: Int): Int =
    start - (start compare end)

  def stepTo(start: Point, end: Point): Point =
    return Point(stepTo(start.x, end.x), stepTo(start.y, end.y))

  def line(start: Point, end: Point): mutable.ListBuffer[Point] =
    val res = mutable.ListBuffer[Point]()

    var current = start
    while (current != end) {
      res += current
      current = stepTo(current, end)
    }

    return res

  def manhattanDistance(p1: Point, p2: Point): Int =
    return abs(p1.x - p2.x) + abs(p1.y - p2.y)

  class Diamond(
      val center: Point,
      val distance: Int,
  ):
    def adjacent(): mutable.ListBuffer[Point] =
      val outerDist = distance + 1
      val top = Point(center.x - outerDist, center.y);
      val right = Point(center.x, center.y + outerDist);
      val bottom = Point(center.x + outerDist, center.y);
      val left = Point(center.x, center.y - outerDist);

      val res =
        line(top, right) ++
          line(right, bottom) ++
          line(bottom, left) ++
          line(left, top)

      return res

    def contains(point: Point): Boolean =
      return manhattanDistance(center, point) <= distance

    override def toString(): String =
      return s"$center -> $distance"

  def isOutside(p: Point, diamonds: mutable.Set[Diamond]): Boolean =
    for (diamond <- diamonds) {
      if (diamond.contains(p)) {
        return false
      }
    }
    return true

  def solveB(input: List[String]): String =
    // val bounds = 20
    val bounds = 4000000 // in the real input

    val diamonds = mutable.Set[Diamond]()

    for (line <- input) {
      val chunks = line.split(": ")
      val sensorChunk = chunks(0)
      val beaconChunk = chunks(1)
      val sensor = Point(slicePrefix(sensorChunk, "Sensor at "))
      val beacon = Point(slicePrefix(beaconChunk, "closest beacon is at "))
      val dist = manhattanDistance(sensor, beacon)

      diamonds.add(Diamond(sensor, dist))
    }

    for (diamond <- diamonds) {
      val res = diamond
        .adjacent()
        .filter(p => p.x >= 0 && p.x <= bounds && p.y >= 0 && p.y <= bounds)
        .find(p => isOutside(p, diamonds))

      if (res.isDefined) {
        return freq(res.get).toString()
      }
    }

    return "B"
