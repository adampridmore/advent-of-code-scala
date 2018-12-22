package Day6

import org.scalatest.FunSuite

import scala.collection.immutable

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
    def parse(line: String): Point = {
      val regex = raw"(\d+), (\d+)".r

      val matches = regex.findAllIn(line)
      val x = matches.group(1).toInt
      val y = matches.group(2).toInt

      Point(x, y)
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

  test("Manhattan distance") {
    assert(Point(1, 1).distance(Point(1, 1)) === 0)
    assert(Point(1, 1).distance(Point(2, 2)) === 2)
  }

  def printGrid(grid: Array[Array[Cell]]): Unit = {
    def lineToText(line: Array[Cell]): String = {

      def cellToString(c: Cell) = {
        c match {
          case EmptyCell() => "."
          case Danger(name) => name.toString
        }
      }

      line
        .map(cellToString)
        .mkString("")
    }

    val text = grid.map(lineToText).mkString("\r\n")

    println(text)
    ()
  }

  test("Draw example data") {
    def intToLetter(i: Int): String = {
      val letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      letters(i).toString
      //i.toString
    }

    val dangerByPoint =
      parseLines(exampleData)
        .zipWithIndex
        .map { case (line, i) => (Point(line.x, line.y), Danger(intToLetter(i))) }
        .toMap

    def asCell(c: Cell): Cell = c

    def coordinateToCell(x: Int, y: Int): Cell = {
      dangerByPoint.get(Point(x, y)) match {
        case Some(danger: Danger) => asCell(danger)
        case None => asCell(EmptyCell())
      }
    }

    val grid: Array[Array[Cell]] =
      (for (x <- 0 to 9) yield {
        (for (y <- 0 to 9) yield {
          coordinateToCell(x, y)
        }).toArray
      }).toArray

    printGrid(grid)
  }
}
