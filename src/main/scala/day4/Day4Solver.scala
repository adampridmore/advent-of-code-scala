//import scala.collection.mutable
package day4

import java.time.{ZoneId, ZonedDateTime}

import scala.collection.mutable

object Day4Solver {
  type Minutes = Array[Status.Value]

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

  def parseDateTime(str: String): SantaDate2 = {

    try {

      val regex = raw"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)".r
      val matches = regex.findAllIn(str)
      val month = matches.group(2).toInt
      val day = matches.group(3).toInt
      val hour = matches.group(4).toInt
      val minute = matches.group(5).toInt

      new SantaDate2(month, day, hour, minute)
    }catch {
      case e:Exception=>throw new RuntimeException("Error on line: " + str, e)
    }
  }

  private def parseGuardId(part: String) = {
    part
      .split(" ")(2)
      .substring(1)
  }

  def parseGuardLines(data: String) = {
    val textLines = data
      .split("\\r\\n") //fromResource("day4/guards.txt").getLines()
      .filter(s=>s.nonEmpty)

    val sortedLines: Seq[GuardLog] =
      textLines
        .map(parseLine)
        .toList
        .sortBy(x => x.timestamp.zonedDateTime.toInstant.getEpochSecond)
    sortedLines
  }

  def processGuardLines(sortedLines: Seq[GuardLog]): mutable.HashMap[DayGuard, Minutes] = {

    def emptyMinutes() : Minutes = Array.fill[Status.Value](60) {
      Status.Awake
    }

    val dayGuardMinutes: mutable.HashMap[DayGuard, Minutes] = new mutable.HashMap[DayGuard, Minutes]()

    var currentGuardId: Option[String] = Option.empty
    var previousEventTimeStamp: ZonedDateTime = null
    var previousState: Status.Value = Status.Asleep

    for (line <- sortedLines) {
      line match {
        case BeginShift(timestamp, guardId) => {
          if (currentGuardId.isDefined) {
            santaMinuteIterator(previousEventTimeStamp, previousEventTimeStamp.plusHours(2))
              .foreach(ts => {
                println(s"$previousState(init) - GuardId: $currentGuardId TS: ${ts.toString}")
                dayGuardMinutes.getOrElseUpdate(DayGuard(ts.getMonthValue, ts.getDayOfMonth, currentGuardId.get), emptyMinutes())(ts.getMinute) = previousState
              })
          }

          currentGuardId = Some(guardId)
          previousEventTimeStamp = timestamp.zonedDateTime
          previousState = Status.Awake
        }
        case FallsAsleep(timestamp) => {
          santaMinuteIterator(previousEventTimeStamp, timestamp.zonedDateTime)
            .foreach(ts => {
              println(s"Awake - GuardId: $currentGuardId TS: ${ts.toString}")
              dayGuardMinutes.getOrElseUpdate(DayGuard(ts.getMonthValue, ts.getDayOfMonth, currentGuardId.get), emptyMinutes)(ts.getMinute) = Status.Awake
            })
          previousEventTimeStamp = timestamp.zonedDateTime
          previousState = Status.Asleep
        }
        case WakesUp(timestamp) => {
          santaMinuteIterator(previousEventTimeStamp, timestamp.zonedDateTime)
            .foreach({
              ts =>
                println(s"Asleep - GuardId: $currentGuardId TS: ${ts.toString}")
                dayGuardMinutes.getOrElseUpdate(DayGuard(ts.getMonthValue, ts.getDayOfMonth, currentGuardId.get), emptyMinutes)(ts.getMinute) = Status.Asleep
            })
          previousEventTimeStamp = timestamp.zonedDateTime
          previousState = Status.Awake
        }
      }
    }

    santaMinuteIterator(previousEventTimeStamp, previousEventTimeStamp.plusHours(1))
      .foreach({
        ts =>
          println(s"$previousState(end) - GuardId: $currentGuardId TS: ${ts.toString}")
          dayGuardMinutes.getOrElseUpdate(DayGuard(ts.getMonthValue, ts.getDayOfMonth, currentGuardId.get), emptyMinutes)(ts.getMinute) = previousState
      })

    dayGuardMinutes
  }


  private def santaMinuteIterator(start: ZonedDateTime, end: ZonedDateTime) = {
    Iterator
      .iterate(start) {
        ts => ts.plusMinutes(1)
      }
      .takeWhile(ts => ts.toInstant.getEpochSecond < end.toInstant.getEpochSecond)
      .filter(ts => ts.getHour == 0)
  }
}

