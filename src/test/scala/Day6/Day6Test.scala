package Day6

import org.scalatest.FunSuite

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

      def cellToString(c: Cell): String = {
        c match {
          case EmptyCell() => "."
          case Danger(name, _) => name.toString
          case Closest(danger) => danger.name.toLowerCase()
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

  def intToLetter(i: Int): String = {
    val letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    letters(i).toString
    //i.toString
  }

  def asCell(c: Cell): Cell = c

  test("Draw example data") {
    val points: Seq[Point] = parseLines(exampleData)
    def getClosestDanger(dangers: Seq[Danger], point: Point): Closest = {
      val (danger, _) =
        dangers
          .map(danger => (danger, danger.point.distance(point)))
          .minBy { case (_, distance) => distance }

      // TODO - If two are closest, return empty
      Closest(danger)
    }

    val dangers = points
      .zipWithIndex
      .map { case (p, i) => Danger(intToLetter(i), p) }

    val dangerByPoint: Map[Point, Danger] =
      dangers
        .map { d => (d.point, d) }
        .toMap

    def coordinateToCell(dangers: Seq[Danger], point: Point): Cell = {
      dangerByPoint.get(point) match {
        case Some(danger: Danger) => danger
        case None => getClosestDanger(dangers, point)
      }
    }

    val grid: Array[Array[Cell]] =
      (for (y <- 0 to 9) yield {
        (for (x <- 0 to 9) yield {
          coordinateToCell(dangers,Point(x, y))
        }).toArray
      }).toArray

    printGrid(grid)
  }
}
