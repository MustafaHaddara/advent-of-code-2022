package solutions

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import solutions.Day

class Day06 extends Day:
  val day = 1;
  val testInput = """mjqjpqmgbljsphdztnvjfqwrcgsmlb"""

  def posOfFirstUnique(line: String, size: Int): Int =
    var pos = size
    while (Set.from(line.slice(pos - size, pos)).size != size) {
      pos += 1
    }
    return pos

  def solveA(input: List[String]): String =
    val line = input(0)
    return posOfFirstUnique(line, 4).toString()

  def solveB(input: List[String]): String =
    val line = input(0)
    return posOfFirstUnique(line, 14).toString()
