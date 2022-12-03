package solutions

import scala.collection.mutable.ArrayBuffer
import solutions.Day

class Day01 extends Day:
  val day = 1;
  val testInput = """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""

  def solveA(input: List[String]): String =
    val totals = ArrayBuffer[Int]()
    var curr = 0
    for (line <- input) {
      if (line == "") {
        totals += curr
        curr = 0
      } else {
        curr += line.toInt
      }
    }
    return totals.max.toString()

  def solveB(input: List[String]): String =
    val totals = ArrayBuffer[Int]()
    var curr = 0
    for (line <- input) {
      if (line == "") {
        totals += curr
        curr = 0
      } else {
        curr += line.toInt
      }
    }
    return totals.sortWith(_ > _).slice(0, 3).sum.toString()
