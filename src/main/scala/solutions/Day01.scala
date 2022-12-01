package solutions

import scala.collection.mutable.ArrayBuffer
import solutions.Day

class Day01 extends Day:
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
        return "B"
