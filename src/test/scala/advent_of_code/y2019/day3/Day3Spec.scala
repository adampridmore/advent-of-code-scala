package advent_of_code.y2019.day3

import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable
import scala.io.Source.fromResource

class Day3Spec extends WordSpec with Matchers {
  def readInput(): (Seq[String], Seq[String]) = {
    val items = fromResource("advent_of_code/y2019/day3/input.txt")
      .getLines().toList
      .map(_.split(","))

    (items.head, items(1))
  }

  case class Position(column: Int, row: Int)(val distance: Int) {
    def manhattanDistance: Int = Math.abs(column) + Math.abs(row)
  }

  trait Cell {
    def getChar: Char
  }

  case class Wire(lineId: Int, distance: Int) extends Cell {
    //    override def getChar: Char = Wire.alphabet(lineId)
    override def getChar: Char = distance.toString.last
  }

  object Wire {
    val alphabet: Array[Char] = "AB".toCharArray
  }

  case object Empty extends Cell {
    override def getChar: Char = ' '
  }

  case class Crosses(combined: Int) extends Cell {
    override def getChar: Char = 'X' //combined.toString.last
  }

  case class Grid(size: Int = 3000) {

    def solve(lines: Seq[Seq[String]]): Grid = {
      lines.zipWithIndex
        .foreach({ case (line, index) => executeLine(line, index) })

      this
    }

    def getCrosses: Seq[Crosses] = {
      //      for {
      //        row <- grid.indices
      //        col <- grid(0).indices
      //        cell = grid(row)(col)
      //        if cell.isInstanceOf[Crosses]
      //        crosses: Crosses = cell.asInstanceOf[Crosses]
      //      } yield Position(col - size / 2, size / 2 - row)(crosses.combined)

      crosses
    }

    private def parseCommand(command: String): (String, Int) = {
      val split = command.splitAt(1)
      (split._1, split._2.toInt)
    }

    def drawPoints(plots: Seq[Position])(lineId: Int): Unit = {
      plots.foreach(plot => {
        //                println(s"plot: $plot")
        val pen: Cell = grid(plot.row)(plot.column) match {
          case cell if cell == Empty => Wire(lineId, plot.distance)
          case Wire(otherLineId, _) if otherLineId == lineId => Wire(lineId, plot.distance)
          case Wire(_, distance) => {
            val c = Crosses(distance + plot.distance)
            println(s"$distance, ${plot.distance}")
            crosses += c
            c
          }
          case cell => throw new RuntimeException("Unexpected cell:" + cell.toString)
        }

        grid(plot.row)(plot.column) = pen
      })
    }

    def drawLineDown(start: Position)(length: Int)(lineId: Int): Unit = {
      drawPoints(for {
        i <- 1 to length
      } yield Position(start.column, start.row + i)(start.distance + i))(lineId)
    }

    def drawLineUp(start: Position)(length: Int)(lineId: Int): Unit = {
      drawPoints(for {
        i <- 1 to length
      } yield Position(start.column, start.row - i)(start.distance + i))(lineId)
    }

    def drawLineRight(start: Position)(length: Int)(lineId: Int): Unit = {
      drawPoints(for {
        i <- 1 to length
      } yield Position(start.column + i, start.row)(start.distance + i))(lineId)
    }

    def drawLineLeft(start: Position)(length: Int)(lineId: Int): Unit = {
      drawPoints(for {
        i <- 1 to length
      } yield Position(start.column - i, start.row)(start.distance + i))(lineId)
    }

    def executeLine(line: Seq[String], lineId: Int): Unit = {
      var currentPosition = Position(size / 2, size / 2)(0)

      line
        .foreach(command => {
          currentPosition = parseCommand(command) match {
            case ("R", i) =>
              drawLineRight(currentPosition)(i)(lineId)
              currentPosition.copy(column = currentPosition.column + i)(currentPosition.distance + i)
            case ("L", i) =>
              drawLineLeft(currentPosition)(i)(lineId)
              currentPosition.copy(column = currentPosition.column - i)(currentPosition.distance + i)
            case ("U", i) =>
              drawLineUp(currentPosition)(i)(lineId)
              currentPosition.copy(row = currentPosition.row - i)(currentPosition.distance + i)
            case ("D", i) =>
              drawLineDown(currentPosition)(i)(lineId)
              currentPosition.copy(row = currentPosition.row + i)(currentPosition.distance + i)
            case _ => throw new RuntimeException(s"Invalid Command: $command")
          }
        })
    }

    private def emptyGrid(): Array[Array[Cell]] = {

      val grid = Array.fill[Cell](size, size)(Empty)

      println("Grid created")

      grid
    }

    def print(): Unit = {
      println(grid
        .map(_.map(_.getChar).mkString)
        .mkString("\n"))
    }

    val grid: Array[Array[Cell]] = emptyGrid()

    private val crosses: mutable.MutableList[Crosses] = mutable.MutableList.empty[Crosses]
  }

  "Part I" should {
    "Example 1" in {
      val line1: Array[String] = "R8,U5,L5,D3".split(",")
      val line2: Array[String] = "U7,R6,D4,L4".split(",")

      val grid = Grid(20)

      grid.solve(List(line1, line2))

      grid.print()

//      val minDistance = grid.getCrosses
//        .map(_.manhattanDistance)
//        .min
//
//      println(s"minDistance: $minDistance")
//
//      minDistance should be(6)
    }

    "Example 1 - don't count crosses" in {
      val line1: Array[String] = "R8,U5,L5,D6".split(",")

      val grid = Grid(20)

      grid.solve(List(line1))

      grid.print()
    }

    "Example 2" in {

      val line1: Array[String] = "R75,D30,R83,U83,L12,D49,R71,U7,L72".split(",")
      val line2: Array[String] = "U62,R66,U55,R34,D71,R55,D58,R83".split(",")

      val grid = Grid(500)

      grid.solve(List(line1, line2))

      val crosses = grid
        .getCrosses

      println(crosses.mkString(","))

//      val distances = crosses.map(_.manhattanDistance)
//
//      println(s"minDistance: ${distances.min}")
//
//      distances.min should be(159)
    }

    "Example 3 - up 5" in {
      val grid = Grid(30)

      grid.executeLine("R8,U5,L5,D3".split(","), 1)

      grid.print
    }
  }

  class Stopwatch {
    private val start = System.nanoTime()

    def print(name: String): Unit = {
      val durationMs = (System.nanoTime() - start) / 1000000

      println(s"$name - ${durationMs}ms")
    }
  }

  "part II" should {
    "Solution" in {

//      val stopwatch = new Stopwatch()
//
//      stopwatch.print("10")
//
//      val (line1, line2) = readInput()
//
//      stopwatch.print("20")
//
//      val grid = Grid(18000)
//
//      stopwatch.print("30")
//
//      val solved = grid.solve(List(line1, line2))
//
//      stopwatch.print("40")
//
//      //      solved.print
//
//      stopwatch.print("50")
//
//      val sol = solved
//        .getCrosses
//        .minBy(_.manhattanDistance)
//        .manhattanDistance
//
//      stopwatch.print("60")
//
//      println(sol)
//
//      sol should be(731)
//
//      stopwatch.print("70")
    }

    "Solution Part 2" ignore {

      val stopwatch = new Stopwatch()

      stopwatch.print("10")

      val (line1, line2) = readInput()

      stopwatch.print("20")

      val grid = Grid(18000)

      stopwatch.print("30")

      val solved = grid.solve(List(line1, line2))

      stopwatch.print("40")

      stopwatch.print("50")

      val sol = solved
        .getCrosses
        .minBy(c => c.combined)

      stopwatch.print("60")

      println("Crosses Distance Combined: " + sol)

      stopwatch.print("70")
    }
  }
}
