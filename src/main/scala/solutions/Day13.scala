package solutions

import scala.collection.mutable

import spray.json.{JsonParser, JsValue, JsArray, JsNumber}
import spray.json.DefaultJsonProtocol._

import solutions.Day

class Day13 extends Day:
  val day = 10;
  val testInput = """[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"""

  def compareArrays(
      left: Array[JsValue],
      right: Array[JsValue],
  ): Option[Boolean] =
    var idx = 0
    val base = if (left.length != right.length) {
      Some(left.length < right.length)
    } else None

    while (idx < left.length && idx < right.length) {
      val innerRes = compare(left(idx), right(idx))

      if (innerRes.isDefined) {
        return innerRes
      }

      idx += 1
    }
    return base

  def compare(left: JsValue, right: JsValue): Option[Boolean] =
    val res = (left, right) match {
      case (leftNum: JsNumber, rightNum: JsNumber) => {
        // two numbers
        if (left == right) {
          None
        } else {
          val leftInt = leftNum.value.intValue
          val rightInt = rightNum.value.intValue

          Some(leftInt < rightInt)
        }
      }
      case (_: JsArray, _: JsArray) => {
        // two arrays

        val leftArray = left.convertTo[Array[JsValue]]
        val rightArray = right.convertTo[Array[JsValue]]

        compareArrays(leftArray, rightArray)
      }
      case (left: JsNumber, right: JsArray) => {
        val leftArray = Array[JsValue](left)
        val rightArray = right.convertTo[Array[JsValue]]

        compareArrays(leftArray, rightArray)
      }
      case (left: JsArray, right: JsNumber) => {
        val leftArray = left.convertTo[Array[JsValue]]
        val rightArray = Array[JsValue](right)

        compareArrays(leftArray, rightArray)
      }
      case (_, _) => None
    }
    return res

  def solveA(input: List[String]): String =
    var i = 0
    var idx = 1
    var sum = 0
    for (i <- 0 to input.length by 3) {
      val left = input(i)
      val right = input(i + 1)
      val r = compare(JsonParser(input(i)), JsonParser(input(i + 1)))

      if (r.getOrElse(false)) {
        sum += idx
      }

      idx += 1
    }
    return sum.toString()

  def solveB(input: List[String]): String =
    val dividerA = "[[2]]"
    val dividerB = "[[6]]"
    val newInput = input
      .appendedAll(List(dividerA, dividerB))
      .filter(p => p != "")
      .map(s => JsonParser(s))
    val sorted = newInput.sortWith((a, b) => compare(a, b).get)

    var indexA = sorted.indexWhere(s => s.toString == dividerA) + 1
    var indexB = sorted.indexWhere(s => s.toString == dividerB) + 1
    return (indexA * indexB).toString
