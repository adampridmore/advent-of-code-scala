package advent_of_code.y2018.day4

import java.time.ZonedDateTime

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day4Solver {
  type Minutes = ListBuffer[Int]

  def parseLine(lineText: String): GuardLog = {
    val parts = lineText.split(raw"\]")

    val timestamp = parseDateTime(parts(0))

    val guardId = parseGuardId(parts(1))

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
    } catch {
      case e: Exception => throw new RuntimeException("Error on line: " + str, e)
    }
  }

  private def parseGuardId(part: String): String = {
    part
      .split(" ")(2)
      .substring(1)
  }

  def parseGuardLines(data: String): Seq[GuardLog] = {
    val textLines = data
      .split("\\r?\\n") //fromResource("advent_of_code.y2018.day4/guards.txt").getLines()
      .filter(s => s.nonEmpty)

    val sortedLines: Seq[GuardLog] =
      textLines
        .map(parseLine)
        .toList
        .sortBy(x => x.timestamp.zonedDateTime.toInstant.getEpochSecond)
    sortedLines
  }

  private def logMessage(str: String): Unit = {
    //println(str)
  }

  def processGuardLines(sortedLines: Seq[GuardLog]): mutable.HashMap[DayGuard, Minutes] = {

    def emptyMinutes(): Minutes = ListBuffer.empty[Int]

    val dayGuardMinutes: mutable.HashMap[DayGuard, Minutes] = new mutable.HashMap[DayGuard, Minutes]()

    var currentGuardId: Option[String] = Option.empty
    var previousEventTimeStamp: ZonedDateTime = null
    var previousState: Status.Value = Status.Asleep

    for (line <- sortedLines) {
      line match {
        case BeginShift(timestamp, guardId) =>
          if (currentGuardId.isDefined) {
            santaMinuteIterator(previousEventTimeStamp, previousEventTimeStamp.plusHours(2))
              .foreach(ts => {
                logMessage(s"$previousState(init) - GuardId: $currentGuardId TS: ${ts.toString}")

                if (previousState == Status.Asleep) {
                  val minutes = dayGuardMinutes.getOrElseUpdate(DayGuard(ts.getMonthValue, ts.getDayOfMonth, currentGuardId.get), emptyMinutes())
                  minutes.append(ts.getMinute)
                }
              })
          }

          currentGuardId = Some(guardId)
          previousEventTimeStamp = timestamp.zonedDateTime
          previousState = Status.Awake
        case FallsAsleep(timestamp) =>
          santaMinuteIterator(previousEventTimeStamp, timestamp.zonedDateTime)
            .foreach(ts => {
              logMessage(s"Awake - GuardId: $currentGuardId TS: ${ts.toString}")
            })
          previousEventTimeStamp = timestamp.zonedDateTime
          previousState = Status.Asleep
        case WakesUp(timestamp) =>
          santaMinuteIterator(previousEventTimeStamp, timestamp.zonedDateTime)
            .foreach({
              ts =>
                logMessage(s"Asleep - GuardId: $currentGuardId TS: ${ts.toString}")
                val minutes = dayGuardMinutes.getOrElseUpdate(DayGuard(ts.getMonthValue, ts.getDayOfMonth, currentGuardId.get), emptyMinutes())
                minutes.append(ts.getMinute)
            })
          previousEventTimeStamp = timestamp.zonedDateTime
          previousState = Status.Awake
      }
    }

    santaMinuteIterator(previousEventTimeStamp, previousEventTimeStamp.plusHours(1))
      .foreach({
        ts =>
          logMessage(s"$previousState(end) - GuardId: $currentGuardId TS: ${ts.toString}")
          if (previousState == Status.Asleep) {
            val minutes = dayGuardMinutes.getOrElseUpdate(DayGuard(ts.getMonthValue, ts.getDayOfMonth, currentGuardId.get), emptyMinutes())
            minutes.append(ts.getMinute)
          }
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

