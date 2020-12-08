import Day07.BaggageParser.Bags

import scala.io.Source
import scala.util.{Try, Using}
import scala.util.parsing.combinator.RegexParsers

object Day07 extends App {

  object BaggageParser extends RegexParsers {
    class ParserError(msg: String) extends RuntimeException(msg)
    type Bags = Map[String, Map[String, Int]]

    private def word: Parser[String] = """[a-z]+""".r ^^ { identity }
    private def number: Parser[Int] = """\d+""".r ^^ { _.toInt }
    private def noBags: Parser[Map[String, Int]] = """no other bags""".r ^^ { _ => Map.empty }
    private def bag: Parser[Map[String, Int]] = (number ~ word ~ word) <~ "bag(s)?".r ^^ { case number ~ w1 ~ w2 => Map(w1 + " " + w2 -> number.toInt) }
    private def someBags: Parser[Map[String, Int]] = repsep(bag, ",") ^^ { bags => bags.reduce(_ ++ _) }
    private def line: Parser[Bags] = word ~ word  ~ "bags contain" ~ (noBags | someBags) ~ ".".r ^^ {
      case w1 ~ w2 ~ _ ~ bags ~ _  => Map(w1 + " " + w2 -> bags)
    }
    private def lines: Parser[Bags] = phrase(rep(line)) ^^ { bags => bags.reduce(_ ++ _) }

    def parseBags(input: String): Try[Bags] =
      parse(lines, input) match {
        case Success(result, _) => scala.util.Success(result)
        case NoSuccess(error, _) => scala.util.Failure(new ParserError(error))
      }
  }

  def countContainingBags(bags: Bags, bag: String): Int = {
    def containsRecursive(b: String): Boolean = {
      bags(b).keySet.contains(bag) || bags(b).keySet.exists(containsRecursive)
    }
    bags.keySet.count(containsRecursive)
  }

  def countRequiredBags(bags: Bags, bag: String): Int = {
    def countRecursive(b: String): Int = {
      1 + bags(b).map { case (s, n) => n * countRecursive(s) }.sum
    }
    countRecursive(bag) - 1
  }

  val bags = for {
    input <- Using(Source.fromURL(getClass.getResource("input-day07.txt")))(_.mkString)
    bags <- BaggageParser.parseBags(input)
  } yield bags

  val solutionPart1 = countContainingBags(bags.get, "shiny gold")
  assert(solutionPart1 == 372, s"part 1: expected solution 372, got $solutionPart1")

  val solutionPart2 = countRequiredBags(bags.get, "shiny gold")
  assert(solutionPart2 == 8015, s"part 2: expected solution 8015, got $solutionPart2")
}
