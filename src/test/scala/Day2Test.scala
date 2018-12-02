class Day2Test extends org.scalatest.FunSuite {
  def hasThreeDuplicates(str: String) : Boolean ={
    hasExactlyXDuplicates(str, 3)
  }

  def hasTwoDuplicates(str: String): Boolean = {
    hasExactlyXDuplicates(str,2)
  }

  private def hasExactlyXDuplicates(str: String, numberOf: Int) = {
    val grouped =
      str
        .toCharArray
        .groupBy(identity)

    grouped.exists({ case (_, items) => items.length == numberOf })
  }

  test("has no duplicates") {
    assert(hasTwoDuplicates("abcdef") === false)
  }

  test("Two duplicates") {
    assert(hasTwoDuplicates("abbcde") === true)
  }

  test("Three duplicates") {
    assert(hasThreeDuplicates("babbc") === true)
  }
}
