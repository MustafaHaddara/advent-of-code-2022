package solutions

trait Day:
  val day: Int
  val testInput: String
  def solveA(input: List[String]): String
  def solveB(input: List[String]): String
