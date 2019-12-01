package advent_of_code.y2019.day1

import org.scalatest.{Matchers, WordSpec}

import scala.io.Source
import scala.io.Source.fromResource

class Day1Spec extends WordSpec with Matchers {
  def calculateFuel(mass: Int) : Int = {
    // take its mass, divide by three, round down, and subtract 2.

    (mass / 3) - 2
  }

  "For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2." in {
      calculateFuel(12) should be(2)
  }

  "For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2." in {
    calculateFuel(14) should be(2)
  }

  "For a mass of 1969, the fuel required is 1969" in {
    calculateFuel(12) should be(2)
  }
  "For a mass of 100756, the fuel required is 33583." in {
    calculateFuel(12) should be(2)
  }

  "For the input file" in {
    val totalFuel =   fromResource("advent_of_code/y2019/day1/input.txt")
      .getLines()
      .toSeq
      .map(x => Integer.parseInt(x))
      .map(x => calculateFuel(x))
      .sum

    println(s"Total fuel: $totalFuel")
  }
}
