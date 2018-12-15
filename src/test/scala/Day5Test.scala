import org.scalatest.{FunSuite, Ignore}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source.fromResource

class Day5Test extends FunSuite {
  def exampleData = "dabAcCaCBAcCcaDA"

  def react(a: Char, b: Char): Boolean = {
    (a.toLower == b.toLower, a.isUpper, b.isUpper) match {
      case (true, case1, case2) if case1 != case2 => true
      case _ => false
    }
  }

  def solver(data: String): String = {

    var skipNext = false
    var wasReaction = false

    var newData: mutable.ListBuffer[Char] = mutable.ListBuffer[Char]()
    var characters: mutable.ListBuffer[Char] = mutable.ListBuffer[Char]()
    data.foreach(c => characters.append(c))
    do {
      wasReaction = false

      characters
        .zip(characters.drop(1)).foreach { case (a, b) =>
        if (skipNext) {
          skipNext = false
        } else {
          if (react(a, b)) {
            skipNext = true
            wasReaction = true
          } else {
            newData.append(a)
          }
        }
      }
      if (!skipNext) {
        newData.append(exampleData.last)
      }

      characters = newData.clone()
      newData = mutable.ListBuffer.empty[Char]

    } while (wasReaction)

    characters.mkString("")
  }

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
    //    val data = "abc"
    //    def solver2 (data: String) = {
    //      data
    //    }
    //
    //    solver2(data)

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

  //@tailrec
  final def applyReaction2(list: List[Char]): List[Char] = {
    list match {
      case head :: Nil => List(head)
      case a :: b :: tail if react(a, b) => applyReaction2(tail)
      case head :: tail => List(head) ::: applyReaction2(tail)
    }
  }

  test("Reaction2 abcd") {
    var reacted = applyReaction2("abcd".toCharArray.toList).mkString("")
    assert(reacted === "abcd")
  }

  test("Reaction2 abBc") {
    var reacted = applyReaction2("abBc".toCharArray.toList).mkString("")
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
    } while (!done)

    dataArray.mkString("")
  }

  test("Reaction2 exampleData") {

    var reactedData = solver2(exampleData)

    println(exampleData + "\r\n" + reactedData)

    assert(reactedData.mkString("") === "dabCBAcaDA")
  }

  ignore("Reaction2 Real data solver") {
    val realData: String = fromResource("day5/data.txt").mkString

    val result = solver2(realData)

    println(s"Length: ${result.length} : $result")

    assert(result.length === 10450)
  }
}
