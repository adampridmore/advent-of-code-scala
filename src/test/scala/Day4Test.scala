import org.scalactic.Fail
import org.scalatest.FunSuite

import scala.util.matching.Regex

//[1518-11-01 00:00] Guard #10 begins shift
//[1518-11-01 00:05] falls asleep
//[1518-11-01 00:25] wakes up
//[1518-11-01 00:30] falls asleep

case class SantaDate(month: Int, day: Int, hour: Int, minute: Int) {}

abstract class GuardLog(val timestamp: SantaDate) {}

case class BeginShift(timestamp2: SantaDate, guardId: String) extends GuardLog(timestamp = timestamp2)

case class FallsAsleep(timestamp2: SantaDate) extends GuardLog(timestamp2)

case class WakesUp(timestamp2: SantaDate) extends GuardLog(timestamp2)

class Day4Test extends FunSuite {
  def parseDateTime(str: String): SantaDate = {
    val regex = raw"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)".r
    val matches = regex.findAllIn(str)
    val month = matches.group(2).toInt
    val day = matches.group(3).toInt
    val hour = matches.group(4).toInt
    val minute = matches.group(5).toInt

    SantaDate(month, day, hour, minute)
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


}

