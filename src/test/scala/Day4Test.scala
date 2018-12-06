import java.time.{ZoneId, ZonedDateTime}

import day4._
import org.scalatest.FunSuite

//[1518-11-01 00:00] Guard #10 begins shift
//[1518-11-01 00:05] falls asleep
//[1518-11-01 00:25] wakes up
//[1518-11-01 00:30] falls asleep


case class SantaDate(month: Int, day: Int, hour: Int, minute: Int) extends Ordered[SantaDate] {
  def compare(that: SantaDate): Int = {
    import scala.math.Ordered.orderingToOrdered
    (this.month, this.day, this.hour, this.minute) compare(that.month, that.day, that.hour, that.minute)
  }
}

class Day4Test extends FunSuite {
  def parseDateTime(str: String): SantaDate2 = {
    val regex = raw"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)".r
    val matches = regex.findAllIn(str)
    val month = matches.group(2).toInt
    val day = matches.group(3).toInt
    val hour = matches.group(4).toInt
    val minute = matches.group(5).toInt

    new SantaDate2(month, day, hour, minute)
  }

  def parseLine(lineText: String): GuardLog = {
    val parts = lineText.split(raw"\]")

    val timestamp = parseDateTime(parts(0))

    val guardId: String = parseGuardId(parts(1))

    lineText match {
      case t if t.contains("falls") => FallsAsleep(timestamp)
      case t if t.contains("wakes") => WakesUp(timestamp)
      case _ => BeginShift(timestamp, guardId)
    }
  }

  private def parseGuardId(part: String) = {
    part
      .split(" ")(2)
      .substring(1)
  }

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
    """[1518-11-01 00:00] Guard #10 begins shift
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
[1518-11-05 00:55] wakes up"""

  test("Scratch") {

    val dt = ZonedDateTime.of(1518, 11, 4,
      0, 3, 0, 0, ZoneId.of("GMT"))

    println(dt)

    val textLines = exampleData.split("\\r\\n") //fromResource("day4/guards.txt").getLines()

    val sortedLines: Seq[GuardLog] =
      textLines
        .map(parseLine)
        .toList
        .sortBy(x => x.timestamp.zonedDateTime.toInstant.getEpochSecond)

    println(sortedLines.mkString("\r\n"))

    val dayGuardMinutes =  day4.Day4Solver.processGuardLines(sortedLines)


    val r =
      dayGuardMinutes
        .toList
        .sortBy(dgm=>(dgm._1.month,dgm._1.day))

    println(r.mkString("\r\n"))
  }
}

