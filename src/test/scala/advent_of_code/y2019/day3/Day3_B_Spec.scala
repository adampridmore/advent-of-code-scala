package advent_of_code.y2019.day3

import org.scalatest.{Matchers, WordSpec}

import scala.io.Source.fromResource

class Day3_B_Spec extends WordSpec with Matchers {
  def readInput(): (Seq[String], Seq[String]) = {
    val items = fromResource("advent_of_code/y2019/day3/input.txt")
      .getLines().toList
      .map(_.split(","))

    (items.head, items(1))
  }

  case class Position(column: Int, row: Int)

  object Position {
    val zero = Position(0, 0)
    implicit def apply(x : (Int, Int)) : Position = {
      Position(x._1, x._2)
    }
  }

  trait Command {
    val distance: Int

    def doOffset(position: Position, offset: Int): Position

    def execute(position: Position): Seq[Position] = {
      for {
        i <- 1 to distance
      } yield doOffset(position, i)
    }
  }

  object Command {
    def apply(text: String): Command = {
      val c = text.head
      val i = text.tail.toInt

      c match {
        case 'R' => Right(i)
        case 'L' => Left(i)
        case 'U' => Up(i)
        case 'D' => Down(i)
      }
    }
  }

  object Commands {
    def toPositions(start: Position)(commands: Seq[Command]) : Seq[Position] = {

      def fn(positions: Seq[Position], commands: Seq[Command]): (Seq[Position], Seq[Command]) = {
        commands match {
          case command :: tailCommands => fn(positions ++ command.execute(positions.last), tailCommands)
          case _ => (positions, Seq.empty)
        }
      }
      fn(Seq(start), commands)._1.tail
    }
  }

  case class Right(distance: Int) extends Command {
    override def doOffset(position: Position, offset: Int): Position = {
      position.copy(column = position.column + offset)
    }
  }

  case class Left(distance: Int) extends Command {
    override def doOffset(position: Position, offset: Int): Position =
      position.copy(column = position.column - offset)
  }

  case class Up(distance: Int) extends Command {
    override def doOffset(position: Position, offset: Int): Position = {
      position.copy(row = position.row - offset)
    }
  }

  case class Down(distance: Int) extends Command {
    override def doOffset(position: Position, offset: Int): Position = {
      position.copy(row = position.row + offset)
    }
  }

  "Parse Commands" should {
    "turns text commands into a seq of commands" in {
      val commands =
        "R8,U5,L5,D3"
          .split(",")
          .map(text => Command(text))

      commands shouldBe List(Right(8), Up(5), Left(5), Down(3))
    }

    "Execute command" should {
      val prevPosition = Position(0, 0)

      "for one Right command" in {
        val newPositions = Right(1).execute(prevPosition)

        newPositions shouldBe Seq[Position]((1, 0))
      }

      "for two right commands" in {
        val newPositions = Right(2).execute(prevPosition)

        newPositions shouldBe Seq[Position]((1, 0), (2, 0))
      }

      "for two left commands" in {
        val newPositions = Left(2).execute(prevPosition)

        newPositions shouldBe Seq[Position]((-1, 0), (-2, 0))
      }

      "for two up commands" in {
        val newPositions = Up(2).execute(prevPosition)

        newPositions shouldBe Seq[Position]((0,-1), (0,-2))
      }

      "for two down commands" in {
        val newPositions = Down(2).execute(prevPosition)

        newPositions shouldBe Seq[Position]((0,1), (0,2))
      }
    }

    "Execute commands" in {
      val commands = List(Right(1), Down(2))

      val start = Position.zero

      Commands.toPositions(start)(commands) shouldBe Seq[Position]((1,0), (1,1),(1,2))
    }
  }
}
