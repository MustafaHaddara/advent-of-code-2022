package solutions

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable
import solutions.Day

class Day08 extends Day:
  val day = 1;
  val testInput = """30373
25512
65332
33549
35390"""

  def solveA(input: List[String]): String =
    // the edges, account for double-counting the corners
    var visible = input.length * 4 - 4
    var seen = mutable.HashSet[Tuple]()

    // start on the left edge
    for (row <- 1 to input(0).length() - 2) {
      var min = input(row)(0).toInt
      for (col <- 1 to input.size - 2) {
        val v = input(row)(col).toInt
        if (v > min) {
          seen.add((row, col))
          min = v
        }
      }
    }
    // right edge
    for (row <- 1 to input(0).length() - 2) {
      var min = input(row)(input(row).size - 1).toInt
      for (col <- input.size - 2 to 1 by -1) {
        val v = input(row)(col).toInt
        if (v > min) {
          seen.add((row, col))
          min = v
        }
      }
    }

    // top edge
    for (col <- 1 to input.size - 2) {
      var min = input(0)(col).toInt
      for (row <- 1 to input(0).length() - 2) {
        val v = input(row)(col).toInt
        if (v > min) {
          seen.add((row, col))
          min = v
        }
      }
    }

    // bottom edge
    for (col <- 1 to input.size - 2) {
      var min = input(input.size - 1)(col).toInt
      for (row <- input(0).length() - 2 to 1 by -1) {
        val v = input(row)(col).toInt
        if (v > min) {
          seen.add((row, col))
          min = v
        }
      }
    }

    return (seen.size + visible).toString()

  def scoreUp(row: Int, col: Int, grid: List[String]): Int =
    val tree = grid(row)(col).toInt

    var score = 0
    for (r <- row + 1 to grid.size - 1) {
      score += 1
      if (grid(r)(col).toInt >= tree) {
        return score
      }
    }
    return score

  def scoreDown(row: Int, col: Int, grid: List[String]): Int =
    val tree = grid(row)(col).toInt

    var score = 0
    for (r <- row - 1 to 0 by -1) {
      score += 1
      if (grid(r)(col).toInt >= tree) {
        return score
      }
    }
    return score

  def scoreRight(row: Int, col: Int, grid: List[String]): Int =
    val tree = grid(row)(col).toInt

    var score = 0
    for (c <- col + 1 to grid(row).size - 1) {
      score += 1
      if (grid(row)(c).toInt >= tree) {
        return score
      }
    }
    return score

  def scoreLeft(row: Int, col: Int, grid: List[String]): Int =
    val tree = grid(row)(col).toInt

    var score = 0
    for (c <- col - 1 to 0 by -1) {
      score += 1
      if (grid(row)(c).toInt >= tree) {
        return score
      }
    }
    return score

  def score(row: Int, col: Int, grid: List[String]): Int =
    return scoreDown(row, col, grid) *
      scoreUp(row, col, grid) *
      scoreRight(row, col, grid) *
      scoreLeft(row, col, grid)

  def solveB(input: List[String]): String =
    var topScenicScore = 0

    for (row <- 0 until input.size) {
      for (col <- 0 until input(row).size) {
        val s = score(row, col, input)
        if (s > topScenicScore) {
          topScenicScore = s
        }
      }
    }

    return topScenicScore.toString()
