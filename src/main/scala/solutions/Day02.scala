package solutions

import scala.collection.immutable.HashMap
import solutions.Day


class Day02 extends Day:
    val day = 1;
    val testInput = """A Y
B X
C Z
"""
// A = rock, B = paper, C = scissors
// X = rock, Y = paper, Z = scissors

    val shapes = HashMap("X" -> 1, "Y" -> 2, "Z" -> 3)
    val matches = HashMap("X" -> "A", "Y" -> "B", "Z" -> "C")
    val winners = HashMap("X" -> "C", "Y" -> "A", "Z" -> "B")

    def outcome(other: String, mine: String): Int = 
        if (matches(mine) == other) {
            return 3
        }
        if (winners(mine) == other) {
            return 6
        }
        return 0

    def score(other: String, mine: String): Int =
        return shapes(mine) + outcome(other, mine)

    def solveA(input: List[String]): String =
        var totals = 0
        for (line <- input) {
            val pairs = line.split(" ")
            val s = score(pairs(0), pairs(1))
            totals += s
        }
        return totals.toString()

    def outcomeB(outcome: String): Int = 
        outcome match
            case "X" => return 0
            case "Y" => return 3
            case "Z" => return 6
    
    val mapping = HashMap("A" -> "X", "B" -> "Y", "C" -> "Z")
    val wins = HashMap("A" -> "Y", "B" -> "Z", "C" -> "X")
    val losses = HashMap("A" -> "Z", "B" -> "X", "C" -> "Y")
    def scoreB(other: String, outcome: String): Int = 
        outcome match
            case "X" => return shapes(losses(other))
            case "Y" => return shapes(mapping(other))
            case "Z" => return shapes(wins(other))

    def solveB(input: List[String]): String =
        var totals = 0
        for (line <- input) {
            val pairs = line.split(" ")
            val s = scoreB(pairs(0), pairs(1)) + outcomeB(pairs(1))
            totals += s
        }
        return totals.toString()
