package advent_of_code.y2018.day4

abstract class GuardLog(val timestamp: SantaDate2) {}

case class BeginShift(timestamp2: SantaDate2, guardId: String) extends GuardLog(timestamp = timestamp2)

case class FallsAsleep(timestamp2: SantaDate2) extends GuardLog(timestamp2)

case class WakesUp(timestamp2: SantaDate2) extends GuardLog(timestamp2)

object Status extends Enumeration {
  val Awake, Asleep = Value
}
