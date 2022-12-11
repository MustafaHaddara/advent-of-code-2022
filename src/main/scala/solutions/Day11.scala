package solutions

import scala.collection.mutable.{Buffer, ArrayBuffer}
import scala.math.floorDiv
import solutions.Day

class Day11 extends Day:
  val day = 10;
  val testInput = """Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"""

  class Monkey(
      val id: Int,
      val items: Buffer[Long],
      val worry: Long => Long,
      val divisibleBy: Int,
      val passTarget: Int,
      val failTarget: Int,
  ):
    var inspections = 0
    def round(others: ArrayBuffer[Monkey], mod: Long => Long) =
      inspections += items.length
      for (item <- items) {
        val newVal = mod(worry(item))
        if (newVal % divisibleBy == 0) {
          others(passTarget).items += newVal
        } else {
          others(failTarget).items += newVal
        }
      }
      items.clear()

    override def toString: String =
      s"$id -> $items, inspected: $inspections"

  def buildOperation(
      argA: String,
      op: String,
      argB: String,
  ): (Long) => Long =
    val func: (Long, Long) => Long = op match
      case "+" => (a, b) => a + b
      case "*" => (a, b) => a * b

    def operation(s: Long): Long =
      val a: Long = argA match
        case "old" => s
        case _     => argA.toLong
      val b: Long = argB match
        case "old" => s
        case _     => argB.toLong

      return func(a, b)

    return operation

  def buildTest(base: String): (Int) => Boolean =
    val b = base.toInt
    return (a) => a % b == 0

  def buildMonkeys(input: List[String]): ArrayBuffer[Monkey] =
    val monkeys = ArrayBuffer[Monkey]()
    for (idx <- 0 until input.length by 7) {
      val monkeySpec = input(idx)
      val itemsSpec = input(idx + 1)
      val opSpec = input(idx + 2)
      val divisibleBySpec = input(idx + 3)
      val trueSpec = input(idx + 4)
      val falseSpec = input(idx + 5)

      val id =
        monkeySpec.slice("Monkey ".length(), monkeySpec.length() - 1).toInt

      val startingItems = itemsSpec
        .slice("  Starting items: ".length(), itemsSpec.length())
        .split(", ")
        .map(s => s.toLong)
        .toBuffer

      val opChunks =
        opSpec.slice("  Operation: new = ".length(), opSpec.length()).split(" ")
      val op = buildOperation(opChunks(0), opChunks(1), opChunks(2))

      val divisibleBy = divisibleBySpec
        .slice("  Test: divisible by ".length(), divisibleBySpec.length())
        .toInt

      val trueTarget = trueSpec
        .slice("    If true: throw to monkey ".length(), trueSpec.length())
        .toInt
      val falseTarget = falseSpec
        .slice("    If false: throw to monkey ".length(), falseSpec.length())
        .toInt

      monkeys += Monkey(
        id,
        startingItems,
        op,
        divisibleBy,
        trueTarget,
        falseTarget,
      )
    }
    return monkeys

  def solveA(input: List[String]): String =
    val monkeys = buildMonkeys(input)

    for (_ <- 1 to 20) {
      for (m <- monkeys) {
        m.round(monkeys, (a: Long) => floorDiv(a, 3))
      }
    }

    return monkeys
      .map(m => m.inspections)
      .toList
      .sorted
      .slice(monkeys.length - 2, monkeys.length)
      .product
      .toString()

  def solveB(input: List[String]): String =
    val monkeys = buildMonkeys(input)
    val commonMultiplier = monkeys.map(m => m.divisibleBy).product

    for (_ <- 1 to 10000) {
      for (m <- monkeys) {
        m.round(monkeys, (a: Long) => a % commonMultiplier)
      }
    }

    return monkeys
      .map(m => m.inspections)
      .toList
      .sorted
      .slice(monkeys.length - 2, monkeys.length)
      .map(m => m.toLong)
      .product
      .toString()
