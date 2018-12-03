import java.util

import scala.io.Source

class Day1Test extends org.scalatest.FunSuite {
  private def frequenciesTotal(inputText: String) = {
    val inputLines = parseInputLines(inputText)

    inputLines.sum
  }

  private def parseInputLines(inputText: String) = {
    inputText
      .split("\\r\\n")
      .map(Integer.parseInt)
  }

  val exampleData =
    """+1
-2
+3
+1"""

  private val data = Source.fromResource("day1/frequencies.txt").mkString

  test("Day1_frequencies_total_example") {
    val sum: Int = frequenciesTotal(exampleData)
    assert(sum == 3)
  }

  test("Day_1_part1") {
    val sum = frequenciesTotal(data)

    println(s"Day 1 Part 1 : Frequencies total: $sum")

    assert(sum === 553)
  }

  def getFirstDuplicate(data: String): Int = {
    val values: Array[Int] = parseInputLines(data)

    val totals = new util.HashSet[Int]()
    var index = 0
    var sum = 0

    do {
      sum = sum + values(index % values.length)

      if (totals.contains(sum)) {
        return sum
      }
      totals.add(sum)

      index = index + 1
    } while (true)

    throw new Exception("none found")
  }

  test("Day_1_part2") {
    val result = getFirstDuplicate(data)

    println(s"Day 1 Part 2 : First duplicate frequency: $result")
  }
}
