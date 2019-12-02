package advent_of_code.y2019.day2

import org.scalatest.{Matchers, WordSpec}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source.fromResource

class Day2Spec extends WordSpec with Matchers {
  def readInputAsIntegers(): ArrayBuffer[Int] = {
    val items = fromResource("advent_of_code/y2019/day2/input.txt")
      .mkString
      .split(',')
      .toSeq
      .map(x => Integer.parseInt(x))

    ArrayBuffer(items: _*)
  }

  def executeInstruction(memory: ArrayBuffer[Int], noun: Int, verb: Int): Any = {

    memory(1) = noun
    memory(2) = verb

    def doOpCode(programCounter: Int, op: (Int, Int) => Int): Any = {
      memory(memory(programCounter + 3)) = op(memory(memory(programCounter + 1)), memory(memory(programCounter + 2)))
    }

    def addOpCode(programCounter: Int): Any = {
      doOpCode(programCounter, (a, b) => a + b)
    }

    def multiplyOpCode(programCounter: Int): Any = {
      doOpCode(programCounter, (a, b) => a * b)
    }

    @tailrec
    def loop(programCounter: Int): Int = {
      memory(programCounter) match {
        case 1 =>
          addOpCode(programCounter)
          loop(programCounter + 4)
        case 2 =>
          multiplyOpCode(programCounter)
          loop(programCounter + 4)
        case 99 => programCounter
        case op => throw new Exception(s"Unexpected op code: $op at $programCounter")
      }
    }

    loop(programCounter = 0)
  }

  "Part I" should {
    "Example 1" in {
      val memory = ArrayBuffer(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50)

      println(memory)

      executeInstruction(memory, noun = 9, verb = 10)

      println(memory)

      memory should be(ArrayBuffer(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50))
    }

    "Solution" in {
      val memory = readInputAsIntegers()
      println(memory)

      // Patch data (from puzzle)
      val noun = 12
      val verb = 2

      executeInstruction(memory, noun, verb)

      println("Day2 part 1 = " + memory(0))
      // 7594646
    }
  }

  "part II" should {
    "Solution" in {

      def solver(noun: Int, verb: Int): Int ={
        val memory = readInputAsIntegers();

        executeInstruction(memory, noun, verb)
        memory(0)
      }

      val toMatch = 19690720

      val (noun, verb)  = (for {
        noun <- 0 to 99
        verb <- 0 to 99
        solution = solver(noun, verb)
        if solution == toMatch
      } yield (noun, verb)).head

      println(s"Noun = $noun, verb = $verb")

      println("Day 2 - part 2 = " + (100 * noun + verb))
      // Solution: 3376
    }
  }
}
