import Day3._

import scala.io.Source.fromResource

class Day3Test extends org.scalatest.FunSuite {
  test("Parse Line") {
    val lineText = "#10 @ 20,30: 40x50"

    val line = parseLine(lineText)
    val expectedLine = Line("10", Point2d(20, 30), Point2d(40, 50))

    assert(line === expectedLine)
  }

  test("Count cloth with two or more claims") {
    var cloth = Array.ofDim[Int](1001, 1001)

    fromResource("day3/claims.txt")
      .getLines()
      .map(parseLine)
      .foreach(line => {
        line.applyLine(cloth)
      })

    println(cloth.flatten.count(cell => cell > 1))
  }

  //  #1 @ 1,3: 4x4
  //  #2 @ 3,1: 4x4
  //  #3 @ 5,5: 2x2
}
