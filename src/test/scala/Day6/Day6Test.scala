package Day6

import java.io

import org.scalatest.FunSuite

import scala.collection.immutable
import scala.io.Source.fromResource

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

  def realData: String = fromResource("day6/Data.txt").mkString

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
          case EmptyCell() => ".."
          case Danger(name, _) => s"${name.toString.padTo(2, " ").mkString}"
          case Closest(danger) => s"${danger.name.toLowerCase().padTo(2, " ").mkString}"
        }
      }

      line
        .map(cellToString)
        .mkString(",")
    }

    val text = grid.map(lineToText).mkString("\r\n")

    println(text)
    ()
  }

  def intToLetter(i: Int): String = {
    //val letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    //letters(i%letters.length).toString
    i.toString
  }

  def asCell(c: Cell): Cell = c

  test("Draw example data") {
    //    val data = exampleData
    //    val gridSize = 10

    val data = realData
    val gridSize = 400

    val points: Seq[Point] = parseLines(data)

    //    val gridSize = points.flatMap(p => List(p.x, p.y))
    //      .max + 1


    def getClosestDanger(dangers: Seq[Danger], point: Point): Cell = {
      val closest: Seq[(Danger, Int)] =
        dangers
          .map(danger => (danger, danger.point.distance(point)))
          .sortBy { case (_, distance) => distance }
          .take(2).toList

      if (closest(0)._2 == closest(1)._2) EmptyCell()
      else Closest(closest.head._1)
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

    val grid: Day6.Grid =
      (for (y <- 0 until gridSize) yield {
        (for (x <- 0 until gridSize) yield {
          coordinateToCell(dangers, Point(x, y))
        }).toArray
      }).toArray

    def cellToName(cell: Cell) = {
      cell match {
        case Closest(d) => d.name
        case d: Danger => d.name
      }
    }

    val results =
      grid.flatten.filter {
        case Closest(_) => true
        case Danger(_, _) => true
        case EmptyCell() => false
      }.groupBy(cellToName)

    val sideCoordinates: Seq[(Int, Int)] =
      (for (y <- 1 until gridSize - 1) yield {
        List((0, y), (gridSize - 1, y))
      }).flatten

    val topTopBottomCoordinates: Seq[(Int, Int)] =
      (for (x <- 1 until gridSize - 1) yield {
        List((x, 0), (x, gridSize - 1))
      }).flatten

    val edges = (sideCoordinates ++ topTopBottomCoordinates)
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
        .maxBy { case (name, cells) => cells.length }

    //Part1 solution: 5975

    println(s"Size: ${finalResult._2.length} Point: ${finalResult._1}")
    println(s"Grid size:$gridSize")

    println("Edges: " + edges.mkString(","))
    printGrid(grid)
  }
}
