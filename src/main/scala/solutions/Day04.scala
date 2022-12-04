package solutions

import scala.collection.mutable.ArrayBuffer
import solutions.Day

class Day04 extends Day:
  val day = 1;
  val testInput = """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
"""

  class Range(str: String):
    val _pair = str.split("-")
    val _a = _pair(0).toInt
    val _b = _pair(1).toInt
    val start = Math.min(_a, _b)
    val end = Math.max(_a, _b)

    override def toString: String =
      s"($start, $end)"

  def solveA(input: List[String]): String =
    var total = 0
    return input.filter(line => fullyContains(line)).length.toString()

  def fullyContains(line: String): Boolean =
    val pair = line.split(",")
    val a = new Range(pair(0))
    val b = new Range(pair(1))

    return (a.start <= b.start && b.end <= a.end) || (b.start <= a.start && a.end <= b.end)

  def solveB(input: List[String]): String =
    var total = 0
    return input.filter(line => partialOverlap(line)).length.toString()

  def partialOverlap(line: String): Boolean = !noOverlap(line)

  def noOverlap(line: String): Boolean =
    val pair = line.split(",")
    val a = new Range(pair(0))
    val b = new Range(pair(1))

    return a.end < b.start || b.end < a.start
