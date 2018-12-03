import scala.util.matching.Regex

case class Point2d(x : Int, y  : Int){}

case class Line(id : String , pos :Point2d, size : Point2d){}

class Day3Test extends org.scalatest.FunSuite {

  def parseLine(lineText: String): Line = {
    val regex = raw"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)".r

    val matches = regex.findAllIn(lineText)

    val id = matches.group(1)
    val xpos = matches.group(2).toInt
    val ypos = matches.group(3).toInt
    val xsize = matches.group(4).toInt
    val ysize = matches.group(5).toInt

    Line(id, Point2d(xpos, ypos), Point2d(xsize, ysize))
  }

  test("Parse Line"){
    val lineText  = "#10 @ 20,30: 40x50"

    val line = parseLine(lineText)
    val expectedLine = Line("10", Point2d(20, 30), Point2d(40, 50))

    assert(line === expectedLine)
  }

  //  #1 @ 1,3: 4x4
  //  #2 @ 3,1: 4x4
  //  #3 @ 5,5: 2x2
}
