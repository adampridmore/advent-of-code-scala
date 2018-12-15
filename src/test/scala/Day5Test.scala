import org.scalatest.FunSuite

import scala.collection.mutable

class Day5Test extends FunSuite {
  def exampleData = "dabAcCaCBAcCcaDA"

  def react(a: Char, b: Char): Boolean = {
    (a.toLower == b.toLower, a.isUpper, b.isUpper) match {
      case (true, case1, case2) if case1 != case2 => true
      case _ => false
    }
  }

  def solver(data: String): String = {
    val characters = data.toList
    val newData = new mutable.ListBuffer[Char]()

    var skipNext = false

    characters
      .zip(characters.drop(1)).foreach { case (a, b) =>

      if (skipNext) {
        skipNext = false
      } else {
        if (react(a, b)) {
          skipNext = true
        } else {
          newData.append(a)
        }
      }
    }
    if (!skipNext) {
      newData.append(exampleData.last)
    }

    newData.mkString("")
  }

  test("React") {
    assert(!react('A', 'B'))
    assert(!react('A', 'A'))
    assert(react('A', 'a'))
  }

  test("Scratch") {
    val solved = solver(exampleData)

    //    assert(newData.mkString("") == "dabAcCaCBAcCcaDA")
    assert(solved.mkString("") == "dabAaCBAcaDA")
    //assert(solved.length == 10)
  }
}
