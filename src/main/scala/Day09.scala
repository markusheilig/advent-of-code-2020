import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day09 extends App {

  def findInvalidNumber(numbers: Seq[Long], preambleLength: Int): Long = {
    numbers.drop(preambleLength).zipWithIndex.collectFirst {
      case (number, index) if {
        val previousNumbers = numbers.slice(index, index + preambleLength)
        !previousNumbers.combinations(2).exists(_.sum == number)
      } => number
    }.get
  }

  def findSummingUp(numbers: Seq[Long], solution: Long): Seq[Long] = {
    @tailrec
    def sum(from: Int, to: Int, acc: Long): (Int, Int) = {
      if (acc == solution) (from, to)
      else if (acc < solution) sum(from, to + 1, acc + numbers(to))
      else sum(from + 1, from + 2, numbers(from + 1))
    }
    val (from, to) = sum(0, 1, acc = 0)
    numbers.slice(from, to)
  }

  val numbers = Using(Source.fromURL(getClass.getResource("input-day09.txt")))(_.mkString).get.split("\\n").map(_.toLong).toVector

  val preambleLength = 25
  val solutionPart1 = findInvalidNumber(numbers, preambleLength)
  assert(solutionPart1 == 144381670, s"part 1: expected solution 144381670, got $solutionPart1")

  val contiguousSet = findSummingUp(numbers, solutionPart1)
  val solutionPart2 = contiguousSet.min + contiguousSet.max
  assert(solutionPart2 == 20532569, s"part 2: expected solution 20532569, got $solutionPart2")
}
