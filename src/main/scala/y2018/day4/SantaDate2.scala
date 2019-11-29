package y2018.day4

import java.time.{ZoneId, ZonedDateTime}

class SantaDate2(val month: Int, val day: Int, val hour: Int, val minute: Int) {
  private val year = 1518
  private val gmt: ZoneId = ZoneId.of("GMT")

  val zonedDateTime: ZonedDateTime = ZonedDateTime.of(year, month, day, hour, minute, 0, 0, gmt)
}
