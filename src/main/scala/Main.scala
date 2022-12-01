package adventofcode

import scala.io.StdIn.readLine
import scala.io.Source
import solutions.Day

val solvers: Vector[Day] = Vector()

@main def main(dayNum: Int, problem: String): Unit = 
    println(s"running day $dayNum problem $problem")

    val input = Source.fromFile(f"inputs/$dayNum%02d.txt").getLines.toList

    val solver = solvers(dayNum - 1)
    problem match
        case "A" => println(solver.solveA(input))
        case "B" => println(solver.solveB(input))
        case _ => println("Invalid problem given")
