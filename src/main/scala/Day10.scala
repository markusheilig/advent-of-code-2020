import scala.annotation.tailrec
import scala.collection.MapView
import scala.io.Source
import scala.util.Using

object Day10 extends App {

  val numbers = Using(Source.fromURL(getClass.getResource("input-day10.txt")))(_.mkString).get.split("\\n").map(_.toInt).toList

  def solve1(xs: List[Int]): Int = {
    val sorted = xs.sorted
    val diffs: MapView[Int, Int] =
      sorted.tail.zip(sorted.init)
       .map { case (x, y) => x - y }
       .groupBy(identity)
       .view.mapValues(_.size)
    (diffs(1) + 1) * (diffs(3) + 1)
  }

  def solve2(input: Seq[Int]): Long = {
    // stolen from here: https://github.com/kbielefe/advent-of-code/blob/master/src/main/scala/2020/10.scala
    val init = Map(input.max + 3 -> 1L)
    val counts = input.appended(0).sorted.reverse.foldLeft(init){case (counts, next) =>
      val count = (next + 1 to next + 3).map(counts.getOrElse(_, 0L)).sum
      counts + (next -> count)
    }
    counts(0)
  }

  val solutionPart1 = solve1(numbers)
  assert(solutionPart1 == 2470, s"part 1: expected solution 2470, got $solutionPart1")

  val solutionPart2 = solve2(numbers)
  assert(solutionPart2 == 1973822685184L, s"part 1: expected solution 2470, got $solutionPart2")
}
