import org.scalatest.FunSuite

case class Point (x: Int, y: Int)

class Day6Test extends FunSuite {
  val exampleData =
    """
1, 1
1, 6
8, 3
3, 4
5, 5
8, 9
"""

  def parseLines(data: String): List[Point] = {
    def parse(line: String) : Point = {
      val parts = line
        .split(",")
        .map(s=>s.trim)
        .map(s=>s.toInt)

      val x = parts(0)
      val y = parts(1)
      Point(x,y)
    }

    exampleData
      .lines
      .filter(s=>s.nonEmpty)
      .map(parse)
      .toList
  }

  test("Scratch") {
    val lines = parseLines(exampleData)

    println(lines.mkString("\r\n"))

  }
}
