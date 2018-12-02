import Day2._

import scala.io.Source

class Day2Test extends org.scalatest.FunSuite {
  test("has no duplicates") {
    assert(hasTwoDuplicates("abcdef") === false)
  }

  test("Two duplicates") {
    assert(hasTwoDuplicates("abbcde") === true)
  }

  test("Three duplicates") {
    assert(hasThreeDuplicates("babbc") === true)
  }

  val exampleData =
    """abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab"""

  test("parse_text_to_string_list") {
    var list = parseTextToLines(exampleData)

    assert(list.length === 7)
    assert(list(0) === "abcdef")
  }

  test("solve_example_day") {
    assert(new Day2(exampleData).solution_part1() === 12)
  }

  test("solve_day_part_1") {
    val data = Source.fromResource("day2/input.txt").mkString

    val result = new Day2(data).solution_part1()

    println(result)
    assert(result === 9139)
  }
}
