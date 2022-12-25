package solutions

import java.util.concurrent.{Executors, ExecutorService}

import scala.math.abs
import scala.collection.Set
import scala.collection.mutable

import solutions.Day
import java.util.concurrent.Future
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Callable
import scala.concurrent.Future.apply

class Day16 extends Day:
  val day = 16;
  val testInput = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"""

  def parseLine(line: String): (String, String, String) =
    val template =
      raw"Valve ([A-Z]{2}) has flow rate=(\d\d?); tunnel(s?) lead(s?) to valve(s?) (.*)".r
    line match {
      case template(id, rate, _, _, _, connections) =>
        return (id, rate, connections)
    }
    return ("", "", "")

  def parseInput(
      input: List[String],
  ): (mutable.Map[String, List[String]], mutable.Map[String, Int]) =
    val adjacencies = mutable.Map[String, List[String]]()
    val flowRates = mutable.Map[String, Int]()

    for (line <- input) {
      val (valveId, valveFlow, connections) = parseLine(line)

      adjacencies.put(valveId, connections.split(", ").toList)
      flowRates.put(valveId, valveFlow.toInt)
    }

    return (adjacencies, flowRates)

  def bestPath(
      start: String,
      end: String,
      adjacencies: mutable.Map[String, List[String]],
  ): Int =
    return bestPath(start, end, adjacencies, Set[String](), 0)

  def bestPath(
      start: String,
      end: String,
      adjacencies: mutable.Map[String, List[String]],
      seen: Set[String],
      currentCost: Int,
  ): Int =
    if (start == end) {
      return currentCost
    }
    var minCost = -1
    for (next <- adjacencies.getOrElse(start, List())) {
      if (!seen.contains(next)) {
        val cost =
          bestPath(next, end, adjacencies, seen + start, currentCost + 1)
        if (cost != -1 && (minCost == -1 || cost < minCost)) {
          minCost = cost
        }
      }
    }
    return minCost

  def findCosts(
      adjacencies: mutable.Map[String, List[String]],
  ): mutable.Map[String, mutable.Map[String, Int]] =
    val costs = mutable.Map[String, mutable.Map[String, Int]]()

    for (start <- adjacencies.keySet) {
      costs.put(start, mutable.Map[String, Int]())
      for (end <- adjacencies.keySet) {
        if (start != end) {
          val cost = bestPath(start, end, adjacencies)
          costs.get(start).get.put(end, cost)
        }
      }
    }

    return costs

  def bestPressure(
      adjacencies: mutable.Map[String, List[String]],
      flowRates: mutable.Map[String, Int],
      nonZero: List[String],
  ): Int =
    var starting = "AA"
    val stepCosts = findCosts(adjacencies)

    return bfsBestPressure(
      stepCosts,
      flowRates,
      nonZero,
      Set[String](),
      starting,
      0,
      30,
      List(starting),
    )

  def bfsBestPressure(
      stepCosts: mutable.Map[String, mutable.Map[String, Int]],
      flowRates: mutable.Map[String, Int],
      nonZero: List[String],
      opened: Set[String],
      currentNode: String,
      currentPressure: Int,
      minutesRemaining: Int,
      path: List[String],
  ): Int =
    if (minutesRemaining < 0) {
      return currentPressure
    }
    var pressure = flowRates.get(currentNode).get * minutesRemaining

    if (opened.size == nonZero.size) {
      return currentPressure + pressure
    }

    var bestPressure = 0
    for (candidate <- nonZero) {
      if (!opened.contains(candidate)) {
        val cost = stepCosts.get(currentNode).get.get(candidate).get + 1
        val nextPressure = bfsBestPressure(
          stepCosts,
          flowRates,
          nonZero,
          opened + candidate,
          candidate,
          currentPressure + pressure,
          minutesRemaining - cost,
          path ++ List(candidate),
        )

        if (nextPressure > bestPressure) {
          bestPressure = nextPressure
        }
      }
    }
    return bestPressure

  def solveA(input: List[String]): String =
    val (adjacencies, flowRates) = parseInput(input)
    val nonZero = flowRates
      .filter((id, value) => value != 0)
      .map((id, value) => id)
      .toList

    return bestPressure(adjacencies, flowRates, nonZero).toString()

  def bestPressurePair(
      adjacencies: mutable.Map[String, List[String]],
      flowRates: mutable.Map[String, Int],
      nonZero: List[String],
  ): Int =
    var starting = "AA"
    val stepCosts = findCosts(adjacencies)
    println(nonZero)

    val split = (nonZero.size / 2).toInt

    var best = 0
    for (setA <- nonZero.combinations(split)) {
      val setB = nonZero.filter(s => !setA.contains(s))

      val scoreA = bfsBestPressure(
        stepCosts,
        flowRates,
        setA,
        Set[String](),
        starting,
        0,
        26,
        List(starting),
      )
      val scoreB = bfsBestPressure(
        stepCosts,
        flowRates,
        setB,
        Set[String](),
        starting,
        0,
        26,
        List(starting),
      )

      val total = scoreA + scoreB
      if (total > best) {
        best = total
      }
    }

    return best

  def solveB(input: List[String]): String =
    val (adjacencies, flowRates) = parseInput(input)
    val nonZero = flowRates
      .filter((id, value) => value != 0)
      .map((id, value) => id)
      .toList

    return bestPressurePair(adjacencies, flowRates, nonZero).toString()
