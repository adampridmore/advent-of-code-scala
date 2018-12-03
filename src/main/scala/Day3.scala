case class Point2d(x: Int, y: Int) {}
case class Line(id: String, pos: Point2d, size: Point2d) {
  def applyLine(cloth: Array[Array[Int]]): Unit = {
    for(x <- pos.x until pos.x + size.x ;
        y <- pos.y until pos.y + size.y ){

      cloth(x)(y) = cloth(x)(y) + 1
    }
  }
}

object Day3 {
  def parseLine(lineText: String): Line = {
    val regex = raw"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)".r

    val matches = regex.findAllIn(lineText)

    val id = matches.group(1)
    val xpos = matches.group(2).toInt
    val ypos = matches.group(3).toInt
    val xsize = matches.group(4).toInt
    val ysize = matches.group(5).toInt

    Line(id, Point2d(xpos, ypos), Point2d(xsize, ysize))
  }
}
