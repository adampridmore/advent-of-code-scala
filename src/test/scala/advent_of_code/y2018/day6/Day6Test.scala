package advent_of_code.y2018.day6

import advent_of_code.y2018.day6._

import org.scalatest.FunSuite

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

  def realData: String = fromResource("advent_of_code/y2018/day6/Data.txt").mkString

  test("Scratch") {
    val lines = Day6.parseLines(exampleData)

    println(lines.mkString("\r\n"))
  }

  test("Manhattan distance") {
    assert(Point(1, 1).distance(Point(1, 1)) === 0)
    assert(Point(1, 1).distance(Point(2, 2)) === 2)
  }

  def asCell(c: Cell): Cell = c

  test("Solve example data part 1") {
    val data = exampleData
    val gridSize = 10

    val (solution, grid) = Day6.solver(data, gridSize)

    println(s"Size: $solution")
    println(s"Grid size:$gridSize")

    Day6.printGrid(grid)

    assert(solution === 17)
  }

  test("Solve real data part 1") {
    val data = realData
    val gridSize = 400

    val (solution, grid) = Day6.solver(data, gridSize)

    println(s"Size: $solution")
    println(s"Grid size:$gridSize")

//    Day6.printGrid(grid)

    //Part1 solution: 5975
    assert(solution === 5975)
  }
}
