import day4.Day4Solver._
import day4.Status._
import day4._
import org.scalatest.FunSuite

import scala.collection.{immutable, mutable}

//[1518-11-01 00:00] Guard #10 begins shift
//[1518-11-01 00:05] falls asleep
//[1518-11-01 00:25] wakes up
//[1518-11-01 00:30] falls asleep

//case class SantaDate(month: Int, day: Int, hour: Int, minute: Int) extends Ordered[SantaDate] {
//  def compare(that: SantaDate): Int = {
//    import scala.math.Ordered.orderingToOrdered
//    (this.month, this.day, this.hour, this.minute) compare(that.month, that.day, that.hour, that.minute)
//  }
//}

class Day4Test extends FunSuite {
  test("Parse guard begins shift line") {
    val line = parseLine("[1518-01-02 03:04] Guard #10 begins shift")

    assert(line.timestamp.month == 1)
    assert(line.timestamp.day == 2)
    assert(line.timestamp.hour == 3)
    assert(line.timestamp.minute == 4)

    line match {
      case x@BeginShift(_, guardId) => assert(guardId == "10")
      case _ => Unit
    }
  }

  test("Parse falls alseep") {
    val line = parseLine("[1518-01-02 03:04] falls asleep")

    line match {
      case FallsAsleep(timestamp) => Unit
      case x => fail(s"Wrong type: ${x.getClass.toString}")
    }

    assert(line.timestamp.month == 1)
    assert(line.timestamp.day == 2)
    assert(line.timestamp.hour == 3)
    assert(line.timestamp.minute == 4)
  }

  test("Parse falls wakes up") {
    val line = parseLine("[1518-11-01 00:25] wakes up")

    line match {
      case WakesUp(timestamp) => Unit
      case x => fail(s"Wrong type: ${x.getClass.toString}")
    }
  }

  def exampleData =
    """
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
"""


  test("Parse guard lines from raw test") {
    def data =
      """
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
"""

    val sortedLines = parseGuardLines(data)

    assert(sortedLines(0).timestamp.month == 11)
    assert(sortedLines(0).timestamp.day == 1)
  }

  test("One guard begins shift and falls asleep and wakes up") {
    def data =
      """[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up"""

    val sortedLines = parseGuardLines(data)

    println(sortedLines.mkString("\r\n"))

    val dayGuardMinutes = processGuardLines(sortedLines)

    val minutes = dayGuardMinutes(DayGuard(11, 1, "10"))
    assert(minutes(0) == Awake)
    assert(minutes(4) == Awake)
    assert(minutes(5) == Asleep)
    assert(minutes(24) == Asleep)
    assert(minutes(25) == Awake)
    assert(minutes(59) == Awake)
  }

  test("Two guards begins shift and falls asleep") {
    def data =
      """
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-02 00:00] Guard #20 begins shift
[1518-11-02 00:10] falls asleep
"""

    val sortedLines = parseGuardLines(data)

    println(sortedLines.mkString("\r\n"))

    val dayGuardMinutes = processGuardLines(sortedLines)

    val guardTen = dayGuardMinutes(DayGuard(11, 1, "10"))
    assert(guardTen(0) == Awake)
    assert(guardTen(4) == Awake)
    assert(guardTen(5) == Asleep)
    assert(guardTen(24) == Asleep)
    assert(guardTen(25) == Awake)
    assert(guardTen(59) == Awake)

    val guardTwenty = dayGuardMinutes(DayGuard(11, 2, "20"))
    assert(guardTwenty(0) == Awake)
    assert(guardTwenty(9) == Awake)
    assert(guardTwenty(10) == Asleep)
    assert(guardTwenty(59) == Asleep)
  }

  test("Scratch") {
    val sortedLines = parseGuardLines(exampleData)

    //    println(sortedLines.mkString("\r\n"))

    val dayGuardMinutes: mutable.Map[DayGuard, Minutes] = processGuardLines(sortedLines)

    val r: (String, Int) = dayGuardMinutes
      .groupBy(dg => dg._1.guardId)
      .map(g => (g._1, g._2.valuesIterator.map(mins => mins.count(m => m == Status.Asleep)).sum))
      .maxBy(x => x._2)

    val guardId = r._1
    val totalAsleepMins = r._2

    println(s"Guard '$guardId' was asleep for $totalAsleepMins minutes.")

    val guardData = dayGuardMinutes.filter(dgm => dgm._1.guardId == guardId)
    val result =
      guardData
        .map({
          gd =>
            gd._2
              .filter(m => m == Status.Asleep)
              .zipWithIndex.map { case (_, minute) => minute }
        })
        .flatten

    //      .filter(m=>m==Status.Asleep)
    //      .zipWithIndex.map{case (element, index) => index}
    //      .groupBy(x=>x)
    //      .maxBy(group=>group._2.toList.length)

    println(result.mkString("\r\n"))

    //    val mostFrequentMin = result._1
    //    println("Most frequent asleep minute: " + mostFrequentMin)
  }
}

