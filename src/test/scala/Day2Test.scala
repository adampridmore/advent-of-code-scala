import scala.io.Source

class Day2Test extends org.scalatest.FunSuite {
  def hasThreeDuplicates(str: String) : Boolean ={
    hasExactlyXDuplicates(str, 3)
  }

  def hasTwoDuplicates(str: String): Boolean = {
    hasExactlyXDuplicates(str,2)
  }

  private def hasExactlyXDuplicates(str: String, numberOf: Int) = {
    val grouped =
      str
        .toCharArray
        .groupBy(identity)

    grouped.exists({ case (_, items) => items.length == numberOf })
  }

  test("has no duplicates") {
    assert(hasTwoDuplicates("abcdef") === false)
  }

  test("Two duplicates") {
    assert(hasTwoDuplicates("abbcde") === true)
  }

  test("Three duplicates") {
    assert(hasThreeDuplicates("babbc") === true)
  }

  val exampleData = """abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab"""

  test("parse_text_to_string_list"){
    var list = parseTextToLines(exampleData)

    assert(list.length === 7)
    assert(list(0) === "abcdef")
  }

  test("solve_example_day"){
    assert(day_2_solver(exampleData) === 12)
  }

  test("solve_day_part_1"){
    val data = Source.fromResource("day2/input.txt").mkString

    val result  = day_2_solver(data)

    println(result)
    assert(result === 9139)
  }

  private def day_2_solver(data: String): Int = {
    val lines = parseTextToLines(data)

    val duplicatesCount = lines.count(hasTwoDuplicates)
    val triplicateCount = lines.count(hasThreeDuplicates)

    val result = duplicatesCount * triplicateCount
    result
  }

  private def parseTextToLines(data : String): Array[String] = {
      data.split("\\r?\\n")
  }
}
