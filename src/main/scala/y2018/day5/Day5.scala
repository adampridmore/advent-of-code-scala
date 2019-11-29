package y2018.day5

import scala.collection.mutable

object Day5 {
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
        newData.append(characters.last)
      }

      characters = newData.clone()
      newData = mutable.ListBuffer.empty[Char]

    } while (wasReaction)

    characters.mkString("")
  }

  def solverPart2(data:String): Int = {
    def removePolymer(c: Char, data: String): String = {
      data.filter(d => d.toLower != c)
    }

    val result =
      (for (polymer <- 'a' to 'z') yield polymer)
        .map(polymer => {
          val filteredData = removePolymer(polymer, data)
          solver(filteredData)
        })
        .map(d => d.length)
        .min
    result
  }
}
