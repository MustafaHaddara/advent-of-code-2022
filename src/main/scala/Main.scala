package adventofcode

import scala.io.StdIn.readLine
import scala.io.Source

import solutions.*

// todo would be cool to fill this in at compile time by reflection
val solvers: Vector[Day] = Vector(
  Day01(),
  Day02(),
  Day03(),
  Day04(),
  Day05(),
  Day06(),
  Day07(),
  Day08(),
  Day09(),
  Day10(),
)

@main def main(dayNum: Int, problem: String, test: Boolean): Unit =
  println(s"running day $dayNum problem $problem")

  val solver = solvers(dayNum - 1)

  val input =
    if (test) solver.testInput.split("\n").toList
    else Source.fromFile(f"inputs/$dayNum%02d.txt").getLines.toList

  problem match
    case "A" => println(solver.solveA(input))
    case "B" => println(solver.solveB(input))
    case _   => println("Invalid problem given")
