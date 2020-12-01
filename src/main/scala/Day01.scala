import scala.io.Source
import scala.util.Using

object Day01 extends App {

  val numbers = Using(Source.fromURL(getClass.getResource("input-day01.txt")))(_.getLines().map(_.toInt).toList).get

  val solutionPart1 = numbers.combinations(2).find(pair => pair.sum == 2020).get.product
  assert(solutionPart1 == 898299, s"part 1: expected solution 898299, got $solutionPart1")

  val solutionPart2 = numbers.combinations(3).find(triple => triple.sum == 2020).get.product
  assert(solutionPart2 == 143933922, s"part 2: expected solution 143933922, got $solutionPart2")

}
