package advent_of_code.Y2018

import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import y2018.Extensions._

class HelpersTest extends FunSuite {
  test("Length extension method") {
    val l = List(1, 2, 3, 4)
    assert(l.myLength2 === 4)
  }

  test("Map with index") {

    val listBuffer = new ListBuffer[Int]

    val l = List(10, 20, 30, 40)

    val mapped = l.mapi {
      case (v, i) => listBuffer.append(v); i
    }

    assert(l.head === 10)
    assert(l(1) === 20)
    assert(mapped.mkString("") === "0123")
  }
}
