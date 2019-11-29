package advent_of_code.y2018.day5

import org.scalatest.FunSuite
import advent_of_code.y2018.day5.Day5
import advent_of_code.y2018.day5.Day5.{react, solver}

import scala.annotation.tailrec
import scala.io.Source.fromResource

class Day5Test extends FunSuite {
  def exampleData: String = "dabAcCaCBAcCcaDA"

  def realData: String = fromResource("day5/data.txt").mkString

  test("React") {
    assert(!react('A', 'B'))
    assert(!react('A', 'A'))
    assert(react('A', 'a'))
  }

  test("Example data solver") {
    val result = solver(exampleData)

    println(s"Length: ${result.length} : $result")

    assert(result.mkString("") == "dabCBAcaDA")
  }

  test("Real data solver") {
    val realData: String = fromResource("day5/data.txt").mkString

    val result = solver(realData)

    println(s"Length: ${result.length} : $result")

    assert(result.length === 10450)
  }

  test("Simple solver") {
    val list = List(1, 2, 3, 4, 5)

    def myMap(list: List[Int]): List[Int] = {
      list match {
        case a :: Nil => List(a * 10)
        case a :: tail => List(a * 10) ::: myMap(tail)
      }
    }

    println(myMap(list).mkString(","))

    def listToString(list: List[Int]): String = {
      list match {
        case a :: Nil => a.toString
        case a :: tail => a.toString + "," + listToString(tail)
      }
    }

    println(listToString(list))
  }

  final def applyReaction2(list: List[Char]): List[Char] = {
    @tailrec
    def apply(acc: List[Char], list: List[Char]): List[Char] = {
      list match {
        case head :: Nil => acc ::: List(head)
        case a :: b :: tail if react(a, b) => apply(acc, tail)
        case head :: tail => apply(acc ::: List(head), tail)
      }
    }

    apply(List.empty[Char], list)
  }

  test("Reaction2 abcd") {
    val reacted = applyReaction2("abcd".toCharArray.toList).mkString("")
    assert(reacted === "abcd")
  }

  test("Reaction2 abBc") {
    val reacted = applyReaction2("abBc".toCharArray.toList).mkString("")
    assert(reacted === "ac")
  }

  def solver2(data: String): String = {
    var dataArray = data.toList
    var done = false

    do {
      val reacted = applyReaction2(dataArray)
      if (reacted == dataArray) {
        done = true
      } else {
        dataArray = reacted
      }

      println(dataArray.length)
    } while (!done)

    dataArray.mkString("")
  }

  test("Reaction2 exampleData") {

    val reactedData = solver2(exampleData)

    println(exampleData + "\r\n" + reactedData)

    assert(reactedData.mkString("") === "dabCBAcaDA")
  }

  ignore("Reaction2 Real data solver") {
    val result = solver2(realData)

    println(s"Length: ${result.length} : $result")

    assert(result.length === 10450)
  }

  test("Day 5 part 2 - Example data") {
    val data = exampleData

    val result: Int = Day5.solverPart2(data)

    println(result)
    assert(result === 4)
  }

  ignore("Day 5 part 2 - Real data") {
    val data = realData

    val result: Int = Day5.solverPart2(data)

    println(result)
    assert(result === 4624)
  }
}
