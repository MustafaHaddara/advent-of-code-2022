package solutions

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import solutions.Day

class Day05 extends Day:
  val day = 1;
  val testInput = """    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"""

  case class Instruction(amount: Int, from: Int, to: Int):
    override def toString: String =
      s"($amount, $from, $to)"

  def splitInput(input: List[String]): Tuple =
    val idx = input.indexWhere(line => { !line.contains("[") })
    val numStacks = input(idx).split("   ").length
    val stacks = input.slice(0, idx)
    val instructions = input.slice(idx + 2, input.length)

    return (numStacks, stacks, instructions)

  def parseInput(input: List[String]): Tuple =
    val (numStacks: Int, stacks: List[String], instructions: List[String]) =
      splitInput(input): @unchecked

    var res = ArrayBuffer.fill(numStacks)(ArrayBuffer[Char]())

    for (line <- stacks) {
      var column = 0
      var idx = 1
      while (column < numStacks) {
        val stack = res(column)
        if (idx < line.length() && line.charAt(idx) != ' ') {
          val c = line.charAt(idx)
          stack += c
        }

        column += 1
        idx += 4
      }
    }

    var parsedInstructions = ArrayBuffer[Instruction]()
    for (line <- instructions) {
      val words = line.split(" ")
      parsedInstructions += Instruction(
        words(1).toInt,
        words(3).toInt - 1,
        words(5).toInt - 1,
      )
    }

    return (res, parsedInstructions)

  def solveA(input: List[String]): String =
    val (
      stacks: ArrayBuffer[ArrayBuffer[Char]],
      instructions: ArrayBuffer[Instruction],
    ) = parseInput(input): @unchecked

    for (instruction <- instructions) {
      var times = 0
      while (times < instruction.amount) {
        val c = stacks(instruction.from).remove(0)
        stacks(instruction.to).insert(0, c)
        times += 1
      }
    }

    return stacks
      .map(arr => arr(0).toString())
      .reduceLeft((a, b) => a.concat(b))

  def solveB(input: List[String]): String =
    val (
      stacks: ArrayBuffer[ArrayBuffer[Char]],
      instructions: ArrayBuffer[Instruction],
    ) = parseInput(input): @unchecked

    for (instruction <- instructions) {
      val src = stacks(instruction.from)
      val (c, newsrc) = src.splitAt(instruction.amount)
      stacks(instruction.from) = newsrc
      stacks(instruction.to) = c ++ stacks(instruction.to)
    }

    return stacks
      .map(arr => arr(0).toString())
      .reduceLeft((a, b) => a.concat(b))
