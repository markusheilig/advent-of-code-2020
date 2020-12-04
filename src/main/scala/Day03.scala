import Day02.getClass

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day03 extends App {

  def countTrees(lines: List[String], right: Int, down: Int): Int = {
    @tailrec
    def count(xs: List[String], pos: Int, acc: Int): Int =
      xs.drop(down) match {
        case Nil => acc
        case h :: t =>
          val nextPos = (pos + right) % h.length
          val isTree = h(nextPos) == '#'
          count(h :: t, pos = nextPos, acc = acc + (if (isTree) 1 else 0))
      }
    count(lines, pos = 0, acc = 0)
  }

  val lines = Using(Source.fromURL(getClass.getResource("input-day03.txt")))(_.getLines().toList).get

  val solutionPart1 = countTrees(lines, 3, 1)
  require(solutionPart1 == 272, s"part 1: expected solution 272, got $solutionPart1")
}
