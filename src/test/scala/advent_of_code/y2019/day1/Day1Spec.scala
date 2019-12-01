package advent_of_code.y2019.day1

import org.scalatest.{Matchers, WordSpec}

import scala.io.Source.fromResource

class Day1Spec extends WordSpec with Matchers {
  def calculateFuel(mass: Int): Int = {
    // take its mass, divide by three, round down, and subtract 2.

    (mass / 3) - 2
  }

  def readInputAsIntegers(): Seq[Int] = {
    fromResource("advent_of_code/y2019/day1/input.txt")
      .getLines()
      .toSeq
      .map(x => Integer.parseInt(x))
  }

  "Part I" should {
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

    "For the input file part 1" in {
      val totalFuel =
        readInputAsIntegers()
          .map(x => calculateFuel(x))
          .sum

      println(s"Dau 1 : Part 1 : Total fuel: $totalFuel")
    }
  }

  "part II" should {
    def calculateFuel2(mass: Int): Int = {
      calculateFuel(mass) match {
        case x if x <= 0 => 0
        case x => x + calculateFuel2(x)
      }
    }

    "For a mass of 1969, the fuel required is 966" in {
      calculateFuel2(1969) should be(966)
    }

    "For a mass of 100756, the fuel required is 50346" in {
      calculateFuel2(100756) should be(50346)
    }

    "For the input file part 2" in {
      val totalFuel =
        readInputAsIntegers()
          .map(x => calculateFuel2(x))
          .sum

      println(s"Day 1 : Part 1 : Total fuel: $totalFuel")
    }
  }
}
