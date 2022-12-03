package solutions

import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import solutions.Day

class Day03 extends Day:
  val day = 1;
  val testInput = """vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"""

  val priorities = ArrayBuffer[Char]('0')
  for (i <- 1 to 26) {
    priorities += (i + 97 - 1).toChar
  }
  for (i <- 1 to 26) {
    priorities += (i + 65 - 1).toChar
  }

  def findCommon(sectionA: String, sectionB: String): Char =
    val seen = Set[Char]()
    for (c <- sectionA) {
      seen.add(c)
    }
    for (c <- sectionB) {
      if (seen.contains(c)) {
        return c
      }
    }
    return '0'

  def solveA(input: List[String]): String =
    var total = 0
    for (line <- input) {
      val breakpoint = line.length() / 2
      val a = line.slice(0, breakpoint)
      val b = line.slice(breakpoint, line.length())
      val next = priorities.indexOf(findCommon(a, b))
      total += next
    }
    return total.toString()

  def solveB(input: List[String]): String =
    var total = 0
    val grouping = ArrayBuffer[String]()
    for (line <- input) {
      grouping += line
      if (grouping.size == 3) {
        val next = priorities.indexOf(findBadge(grouping))
        total += next
        grouping.clear()
      }
    }
    return total.toString()

  def findBadge(grouping: ArrayBuffer[String]): Char =
    val seenA = Set[Char]()
    val seenB = Set[Char]()
    for (c <- grouping(0)) {
      seenA.add(c)
    }
    for (c <- grouping(1)) {
      if (seenA.contains(c)) {
        seenB.add(c)
      }
    }
    for (c <- grouping(2)) {
      if (seenB.contains(c)) {
        return c
      }
    }
    return '0'
