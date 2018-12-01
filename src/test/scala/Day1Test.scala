import java.util

class Day1Test extends org.scalatest.FunSuite {
  test("Day1_frequencies_total_example") {

    val inputText = """+1
-2
+3
+1"""

    val inputLines =
      inputText
      .split("\\r\\n")
      .map(Integer.parseInt)

    val sum = inputLines.sum
    println(sum)
  }
}
