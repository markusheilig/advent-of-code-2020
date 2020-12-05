import scala.io.Source
import scala.util.Using

object Day05 extends App {

  val codes = Using(Source.fromURL(getClass.getResource("input-day05.txt")))(_.getLines().toList).get

  def getSeatId(code: String): Int = {
    def lowerHalf(range: (Int, Int)): (Int, Int) = (range._1, (range._1 + range._2 + 1) / 2)
    def upperHalf(range: (Int, Int)): (Int, Int) = lowerHalf(range.swap).swap

    val (rowCode, columnCode) = code.splitAt(7)

    val row = rowCode.foldLeft((0, 127)) {
      case (range, 'F') => lowerHalf(range)
      case (range, 'B') => upperHalf(range)
    }._1
    val column = columnCode.foldLeft((0, 7)) {
      case (range, 'R') => upperHalf(range)
      case (range, 'L') => lowerHalf(range)
    }._1

    row * 8 + column
  }

  val seatIds = codes.map(getSeatId)

  val solutionPart1 = seatIds.max
  assert(solutionPart1 == 922, s"part 1: expected solution 922, got $solutionPart1")

  val solutionPart2 = ((seatIds.min to seatIds.max) diff seatIds).head
  assert(solutionPart2 == 747, s"part 2: expected solution 747, got $solutionPart2")
}
