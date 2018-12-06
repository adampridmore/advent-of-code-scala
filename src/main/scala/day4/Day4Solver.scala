//import scala.collection.mutable
package day4

import java.time.{ZoneId, ZonedDateTime}

import scala.collection.mutable

object Day4Solver {
  type Minutes = Array[Status.Value]

  def processGuardLines(sortedLines: Seq[GuardLog]): mutable.HashMap[DayGuard, Minutes] = {

    val emptyMinutes: Minutes = Array.fill[Status.Value](60) {
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
            santaMinuteIterator(previousEventTimeStamp, previousEventTimeStamp.plusHours(1))
              .foreach(ts => {
                println(s"$previousState - GuardId: $currentGuardId TS: ${ts.toString}")
                dayGuardMinutes.getOrElseUpdate(DayGuard(ts.getMonthValue, ts.getDayOfMonth, guardId), emptyMinutes)(ts.getMinute) = Status.Awake
              })
            previousEventTimeStamp = timestamp.zonedDateTime
          }

          currentGuardId = Some(guardId)
          previousEventTimeStamp = timestamp.zonedDateTime
          previousState = Status.Awake
        }
        case FallsAsleep(timestamp) => {
          santaMinuteIterator(previousEventTimeStamp, timestamp.zonedDateTime)
            .foreach(ts => {
              println(s"Asleep - GuardId: $currentGuardId TS: ${ts.toString}")
              dayGuardMinutes.getOrElseUpdate(DayGuard(ts.getMonthValue, ts.getDayOfMonth, currentGuardId.get), emptyMinutes)(ts.getMinute) = Status.Asleep
            })
          previousEventTimeStamp = timestamp.zonedDateTime
          previousState = Status.Asleep
        }
        case WakesUp(timestamp) => {
          santaMinuteIterator(previousEventTimeStamp, timestamp.zonedDateTime)
            .foreach({
              ts =>
                println(s"Awake  - GuardId: $currentGuardId TS: ${ts.toString}")
                dayGuardMinutes.getOrElseUpdate(DayGuard(ts.getMonthValue, ts.getDayOfMonth, currentGuardId.get), emptyMinutes)(ts.getMinute) = Status.Awake
            })
          previousEventTimeStamp = timestamp.zonedDateTime
          previousState = Status.Awake
        }
      }
    }

    santaMinuteIterator(previousEventTimeStamp, previousEventTimeStamp.plusHours(1))
      .foreach({
        ts =>
          println(s"$previousState - GuardId: $currentGuardId TS: ${ts.toString}")
          dayGuardMinutes.getOrElseUpdate(DayGuard(ts.getMonthValue, ts.getDayOfMonth, currentGuardId.get), emptyMinutes)(ts.getMinute) = Status.Asleep
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

