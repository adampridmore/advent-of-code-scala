import java.util

import Day3._

import scala.io.Source.fromResource

class Day3Test extends org.scalatest.FunSuite {
  test("Parse Line") {
    val lineText = "#10 @ 20,30: 40x50"

    val line = parseLine(lineText)
    val expectedLine = Line("10", Point2d(20, 30), Point2d(40, 50))

    assert(line === expectedLine)
  }

  private def emptyCloth = {
    Array.fill[Cell](1000, 1000)(Cell(List.empty))
  }

  test("Count cloth with two or more claims") {
    var cloth = emptyCloth

    fromResource("day3/claims.txt")
      .getLines()
      .map(parseLine)
      .foreach(line => {
        line.applyTo(cloth)
      })

    val overlapCount = cloth.flatten.count(cell => cell.ids.size > 1)
    println(overlapCount)
    assert(overlapCount == 118840)
  }

  test("Non-overlapping claim") {
    val cloth = emptyCloth

    val lines =
      fromResource("day3/claims.txt")
        .getLines()
        .map(parseLine)
        .toList

    lines
      .foreach(line => line applyTo cloth)

    val allIds = lines.map(line => line.id).toSet

    val allInvalidIds =
      cloth
        .flatten
        .filter(cell => cell.ids.size > 1).flatMap(c => c.ids)
        .toSet

    val result = (allIds -- allInvalidIds).toList.head
    println(result)
    assert(result == "919")
  }
}
