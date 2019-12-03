package advent_of_code.y2019.day3

import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable
import scala.io.Source.fromResource

/*
--- Day 3: Crossed Wires ---
The gravity assist was successful, and you're well on your way to the Venus refuelling station. During the rush back on Earth, the fuel management system wasn't completely installed, so that's next on the priority list.

Opening the front panel reveals a jumble of wires. Specifically, two wires are connected to a central port and extend outward on a grid. You trace the path each wire takes as it leaves the central port, one wire per line of text (your puzzle input).

The wires twist and turn, but the two wires occasionally cross paths. To fix the circuit, you need to find the intersection point closest to the central port. Because the wires are on a grid, use the Manhattan distance for this measurement. While the wires do technically cross right at the central port where they both start, this point does not count, nor does a wire count as crossing with itself.

For example, if the first wire's path is R8,U5,L5,D3, then starting from the central port (o), it goes right 8, up 5, left 5, and finally down 3:

...........
...........
...........
....+----+.
....|....|.
....|....|.
....|....|.
.........|.
.o-------+.
...........
Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6, down 4, and left 4:

...........
.+-----+...
.|.....|...
.|..+--X-+.
.|..|..|.|.
.|.-X--+.|.
.|..|....|.
.|.......|.
.o-------+.
...........
These wires cross at two locations (marked X), but the lower-left one is closer to the central port: its distance is 3 + 3 = 6.

Here are a few more examples:

R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135
*/

class Day3Spec extends WordSpec with Matchers {
  def readInput(): (Seq[String], Seq[String]) = {
    val items = fromResource("advent_of_code/y2019/day3/input.txt")
      .getLines().toList
      .map(row => row.split(","))

    (items(0), items(1))
  }

  case class Position(column: Int, row: Int) {
    def manhattanDistance: Int = Math.abs(column) + Math.abs(row)
  }

  case class Grid(size: Int = 3000) {

    type Cell = Char

    def solve(lines: Seq[Seq[String]]): Grid = {
      lines.zipWithIndex
        .foreach({ case (line, index) => executeLine(line, index.toString.charAt(0)) })

      this
    }

    def getCrosses(): Seq[Position] = {
      for {
        row <- 0 until grid.length
        col <- 0 until grid(0).length
        if grid(row)(col) == crossCell
      } yield Position(col - size / 2, size / 2 - row)
    }

    private def parseCommand(command: String): (String, Int) = {
      val split = command.splitAt(1)
      (split._1, split._2.toInt)
    }

    def drawPoints(plots: Seq[Position])(lineCharacter: Char): Unit = {
      plots.foreach(plot => {
        //                println(s"plot: $plot")
        val pen = grid(plot.row)(plot.column) match {
          case cell if cell == emptyCell => lineCharacter
          case cell if cell == lineCharacter => lineCharacter
          case _ => crossCell
        }

        grid(plot.row)(plot.column) = pen
      })
    }

    def drawLineDown(start: Position)(length: Int)(lineCharacter: Char) = {
      drawPoints(for {
        i <- start.row + 1 to (start.row + length)

      } yield Position(start.column, i))(lineCharacter)
    }

    def drawLineUp(start: Position)(length: Int)(lineCharacter: Char) = {
      drawPoints(for {
        i <- (start.row - length) until start.row

      } yield Position(start.column, i))(lineCharacter)
    }

    def drawLineRight(start: Position)(length: Int)(lineCharacter: Char) = {
      drawPoints(for {
        i <- start.column + 1 to (start.column + length)
      } yield Position(i, start.row))(lineCharacter)
    }

    def drawLineLeft(start: Position)(length: Int)(lineCharacter: Char) = {
      drawPoints(for {
        i <- (start.column - length) until start.column
      } yield Position(i, start.row))(lineCharacter)
    }

    def executeLine(line: Seq[String], lineCharacter: Char) = {

      var currentPosition = Position(size / 2, size / 2)

      line
        .foreach(command => {
          currentPosition = parseCommand(command) match {
            case ("R", i) => drawLineRight(currentPosition)(i)(lineCharacter); currentPosition.copy(column = currentPosition.column + i)
            case ("L", i) => drawLineLeft(currentPosition)(i)(lineCharacter); currentPosition.copy(column = currentPosition.column - i)
            case ("U", i) => drawLineUp(currentPosition)(i)(lineCharacter); currentPosition.copy(row = currentPosition.row - i)
            case ("D", i) => drawLineDown(currentPosition)(i)(lineCharacter); currentPosition.copy(row = currentPosition.row + i)
            case _ => throw new RuntimeException(s"Invalid Command: $command")
          }
        })
    }

    private def emptyGrid() = {

      val grid = Array.fill(size, size)(emptyCell)

      println("Grid created")

      grid
    }

    def print = {
      println(grid
        .map(row => row.mkString(""))
        .mkString("\n"))
    }

    val emptyCell: Cell = '.'
    val crossCell: Cell = 'x'

    val grid = emptyGrid()
  }

  "Part I" should {
    "Example 1" in {
      //      val (line1, line2) = readInput()
      val line1: Array[String] = "R8,U5,L5,D3".split(",")
      val line2: Array[String] = "U7,R6,D4,L4".split(",")

      val grid = Grid(20)

      grid.solve(List(line1, line2))

      grid.print

      val minDistance = grid.getCrosses()
        .map(x => x.manhattanDistance)
        .min

      println(s"minDistance: $minDistance")

      minDistance should be(6)
    }

    "Example 1 - don't count crosses" in {
      //      val (line1, line2) = readInput()
      val line1: Array[String] = "R8,U5,L5,D6".split(",")

      val grid = Grid(20)

      grid.solve(List(line1))

      grid.print

      //      val minDistance = grid.getCrosses()
      //        .map(x => x.manhattanDistance)
      //        .min
      //
      //      println(s"minDistance: $minDistance")
      //
      //      minDistance should be (159)
    }

    "Example 2" in {

      val line1: Array[String] = "R75,D30,R83,U83,L12,D49,R71,U7,L72".split(",")
      val line2: Array[String] = "U62,R66,U55,R34,D71,R55,D58,R83".split(",")

      val grid = Grid(500)

      grid.solve(List(line1, line2))

      //      grid.print

      val crosses = grid
        .getCrosses()

      println(crosses.mkString(","))

      val distances = crosses.map(x => x.manhattanDistance)

      println(s"minDistance: ${distances.min}")

      // 159
    }


  }

  "part II" should {
    "Solution" in {
      val (line1, line2) = readInput()

      val sol = Grid(18000)
        .solve(List(line1, line2))
        .getCrosses()
        .map(x => x.manhattanDistance)
        .min

      println(sol)
    }
  }
}
