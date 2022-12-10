package solutions

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable
import solutions.Day

class Day10 extends Day:
  val day = 10;
  val testInput = """addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"""

  def solveA(input: List[String]): String =
    var x = 1
    var cycles = 0

    var res = 0

    for (line <- input) {
      val pair = line.split(" ")
      val ins = pair(0)
      val arg = if (pair.length > 1) pair(1) else ""

      ins match
        case "addx" => {
          cycles += 1
          if (cycles % 40 == 20) {
            res += (cycles * x)
          }
          cycles += 1
          if (cycles % 40 == 20) {
            res += (cycles * x)
          }
          x += arg.toInt
        }
        case "noop" => {
          cycles += 1
          if (cycles % 40 == 20) {
            res += (cycles * x)
          }
        }
    }
    return res.toString()

  def solveB(input: List[String]): String =
    var buffer = ArrayBuffer.fill(6) { ArrayBuffer.fill(40) { " " } }

    var x = 1
    var cycles = 0

    for (line <- input) {
      val pair = line.split(" ")
      val ins = pair(0)
      val arg = if (pair.length > 1) pair(1) else ""

      var spriteLeft = x - 1
      var spriteRight = x + 1

      var pos = (cycles % 40)

      ins match
        case "addx" => {
          pos = (cycles % 40)
          if (spriteLeft <= pos && pos <= spriteRight) {
            buffer(cycles / 40)(pos) = "#"
          }
          cycles += 1

          pos = (cycles % 40)
          if (spriteLeft <= pos && pos <= spriteRight) {
            buffer(cycles / 40)(pos) = "#"
          }
          cycles += 1
          x += arg.toInt
        }
        case "noop" => {
          pos = (cycles % 40)
          if (spriteLeft <= pos && pos <= spriteRight) {
            buffer(cycles / 40)(pos) = "#"
          }
          cycles += 1
        }
    }

    var res = ""
    for (row <- buffer) {
      for (c <- row) {
        res += c
      }
      res += "\n"
    }

    return res
