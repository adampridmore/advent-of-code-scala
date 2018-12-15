import org.scalatest.FunSuite

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
    data.foreach(c=>characters.append(c))

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

  test("Real data solver"){
    val realData: String = fromResource("day5/data.txt").mkString

    val result = solver(realData)

    println(s"Length: ${result.length} : $result")
  }
}
