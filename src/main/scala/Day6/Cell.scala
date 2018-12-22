package Day6

sealed trait Cell{}

case class Danger(name: String) extends Cell

case class EmptyCell() extends Cell

case class Point(x: Int, y: Int) {
  def distance(point: Point): Int = {
    Math.abs(x - point.x) + Math.abs(y - point.y)
  }
}
