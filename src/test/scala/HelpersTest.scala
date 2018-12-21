import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

class HelpersTest extends FunSuite {

  object MyExtensions {

    class RichList(list: List[Int]) {
      def myLength2: Int = list.length

      def mapi[T](fn: (Int, Int) => T): List[T] = {
        list.zipWithIndex.map { case (a, b) => fn(a, b) }
      }
    }

    implicit def richList(list: List[Int]): RichList = new RichList(list)
  }

  def myLength(l: List[Int]): Int = {
    l.length
  }

  test("Scratch implicit conversions") {
    import MyExtensions.richList

    val l = List(1, 2, 3, 4)
    val myL = myLength(l)
    l.myLength2
  }

  test("Scratch mapi") {
    import MyExtensions.richList

    val listBuffer = new ListBuffer[Int]

    val l = List(10, 20, 30, 40)

    var mapped = l.mapi {
      case (v, i) => listBuffer.append(v); i
    }.mkString("")

    assert(l.head === 10)
    assert(l(1) === 20)
    assert(mapped === "0123")
  }
}
