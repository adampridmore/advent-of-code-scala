package advent_of_code.y2018.day6

import advent_of_code.y2018.day6.Day6._

object Day6 {
  type Grid = Array[Array[Cell]]

  def parseLines(data: String): List[Point] = {
    def parse(line: String): Point = {
      val regex = raw"(\d+), (\d+)".r

      val matches = regex.findAllIn(line)
      val x = matches.group(1).toInt
      val y = matches.group(2).toInt

      Point(x, y)
    }

    data
      .lines
      .filter(_.nonEmpty)
      .map(parse)
      .toList
  }

  private def cellToString(c: Cell): String = {
    c match {
      case EmptyCell() => ".."
      case Danger(name, _) => s"${name.toString.padTo(2, " ").mkString}"
      case Closest(danger) => s"${danger.name.toLowerCase().padTo(2, " ").mkString}"
    }
  }

  def printGrid(grid: Array[Array[Cell]]): Unit = {
    def lineToText(line: Array[Cell]): String = {
      line
        .map(cellToString)
        .mkString(",")
    }

    val text = grid.map(lineToText).mkString("\r\n")

    println(text)
    ()
  }

  private def getClosestDanger(dangers: Seq[Danger], point: Point): Cell = {
    val closest: Seq[(Danger, Int)] =
      dangers
        .map(danger => (danger, danger.point.distance(point)))
        .sortBy { case (_, distance) => distance }
        .take(2).toList

    if (closest(0)._2 == closest(1)._2) EmptyCell()
    else Closest(closest.head._1)
  }

  def getEdgeCoordinates(gridSize: Int): Seq[(Int, Int)] = {
    val sideCoordinates: Seq[(Int, Int)] =
      (for (y <- 1 until gridSize - 1) yield {
        List((0, y), (gridSize - 1, y))
      }).flatten

    val topTopBottomCoordinates: Seq[(Int, Int)] =
      (for (x <- 1 until gridSize - 1) yield {
        List((x, 0), (x, gridSize - 1))
      }).flatten

    sideCoordinates ++ topTopBottomCoordinates
  }

  def solver(data: String, gridSize: Int): (Int, Grid) = {
    val points: Seq[Point] = Day6.parseLines(data)

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

    val grid: Day6.Grid =
      (for (y <- 0 until gridSize) yield {
        (for (x <- 0 until gridSize) yield {
          coordinateToCell(dangers, Point(x, y))
        }).toArray
      }).toArray

    val results =
      grid.flatten.filter {
        case Closest(_) => true
        case Danger(_, _) => true
        case EmptyCell() => false
      }.groupBy(cellToName)

    val edges = getEdgeCoordinates(gridSize)
      .map { case (x, y) => grid(y)(x) }
      .filter(cell => cell match {
        case EmptyCell() => false
        case _ => true
      })
      .distinct
      .map(cellToName)
      .toSet

    val finalResult =
      results
        .filterNot { case (name: String, _) => edges.contains(name) }
        .maxBy { case (_, cells) => cells.length }

    val solution = finalResult._2.length

    (solution, grid)
  }

  private def intToLetter(i: Int): String = {
    //val letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    //letters(i%letters.length).toString
    i.toString
  }

  private def cellToName(cell: Cell) = {
    cell match {
      case Closest(d) => d.name
      case d: Danger => d.name
    }
  }
}
