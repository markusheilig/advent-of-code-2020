import scala.io.Source
import scala.util.Using

object Day02 extends App {

  val linePattern = "(\\d+)-(\\d+) (\\w): (\\w+)".r

  object part1 {
    def isValidPassword(line: String): Boolean = {
      val linePattern(min, max, char, password) = line
      (min.toInt to max.toInt) contains password.count(_ == char.head)
    }
  }

  object part2 {
    def isValidPassword(line: String): Boolean = {
      val linePattern(pos1, pos2, char, password) = line
      password(pos1.toInt - 1) == char.head ^ password(pos2.toInt - 1) == char.head
    }
  }

  val lines = Using(Source.fromURL(getClass.getResource("input-day02.txt")))(_.getLines().toList).get

  val solutionPart1 = lines.count(part1.isValidPassword)
  assert(solutionPart1 == 614, s"part 1: expected solution 614, got $solutionPart1")

  val solutionPart2 = lines.count(part2.isValidPassword)
  assert(solutionPart2 == 354, s"part 2: expected solution 354, got $solutionPart2")

}
