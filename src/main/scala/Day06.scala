import scala.io.Source
import scala.util.Using

object Day06 extends App {

  val input = Using(Source.fromURL(getClass.getResource("input-day06.txt")))(_.mkString).get

  val groups = input.split("\\n\\n").map(group => group.split("\\n").toList).toList

  def countDistinctChoices(answers: List[String]): Int = answers.mkString.distinct.length
  def countSameChoices(answers: List[String]): Int = answers.reduce(_ intersect _).length

  val solutionPart1 = groups.map(countDistinctChoices).sum
  assert(solutionPart1 == 6351, s"part 1: expected solution 6351, got $solutionPart1")

  val solutionPart2 = groups.map(countSameChoices).sum
  assert(solutionPart2 == 3143, s"part 2: expected solution 3143, got $solutionPart2")
}
