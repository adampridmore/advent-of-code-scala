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

  private val realData = Source.fromResource("day2/input.txt").mkString

  test("parse_text_to_string_list") {
    val list = parseTextToLines(exampleData)

    assert(list.length === 7)
    assert(list(0) === "abcdef")
  }

  test("solve_example_day") {
    assert(new Day2(exampleData).solution_part1() === 12)
  }

  test("solve_day_part_1") {
    val result = new Day2(realData).solution_part1()

    println(s"Day 2 part 1 : $result")
    assert(result === 9139)
  }

  def areSimilar(str1: String, str2: String): Boolean = {
    val differentCount =
      str1
      .zip(str2)
      .count({case(c1, c2) => c1 != c2})

    differentCount == 1
  }

  test("two_strings_that_are_not_similar"){
    assert(areSimilar("abcde", "wvxyz") === false)
  }

  test("two_strings_that_are_similar"){
    assert(areSimilar("fghij", "fguij") === true)
  }

  test("Scratch"){
    def stripCharacterAt(index: Int, str: String): String ={
      str.substring(0, index) +
        (if (index < str.length) str.substring(index+1)
        else "")
    }

    val data = parseTextToLines(realData)

    for(i <- 0 to data(0).length){
      val sortedData = data
        .map(s => stripCharacterAt(i, s))
        .sortBy(identity)

      val result =
        sortedData.zip(sortedData.drop(1))
          .filter({case(a,b) => a === b})

      if (result.nonEmpty) println("Day 2 Part 2 : " + result(0)._1)
    }
  }
}
