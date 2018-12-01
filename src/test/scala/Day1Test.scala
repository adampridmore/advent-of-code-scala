import java.util

import scala.io.Source

class Day1Test extends org.scalatest.FunSuite {
  private def frequenciesTotal(inputText: String) = {
    val inputLines =
      inputText
        .split("\\r\\n")
        .map(Integer.parseInt)

    inputLines.sum
  }

  test("Day1_frequencies_total_example") {

    val inputText =
      """+1
-2
+3
+1"""

    val sum: Int = frequenciesTotal(inputText)
    println(sum)
  }


  test("Day_1_part1") {
    var lines = Source.fromResource("readme.txt").getLines()
    println(lines.mkString(","))

    val data = Source.fromResource("day1/frequencies.txt").mkString

    val sum = frequenciesTotal(data)

    println(sum)

    assert(sum === 553)
  }
}
