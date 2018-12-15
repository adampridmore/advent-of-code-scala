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
      val regex = raw"(\d+), (\d+)".r

      val matches = regex.findAllIn(line)
      val x = matches.group(1).toInt
      val y = matches.group(2).toInt

      Point(x,y)
    }

    exampleData
      .lines
      .filter(_.nonEmpty)
      .map(parse)
      .toList
  }

  test("Scratch") {
    val lines = parseLines(exampleData)

    println(lines.mkString("\r\n"))
  }
}
