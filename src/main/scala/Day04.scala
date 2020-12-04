import scala.io.Source
import scala.util.Using

object Day04 extends App {

  type Passport = Map[String, String]

  def parsePassports(input: String): List[Passport] = {
    def parsePassport(data: String): Passport = {
      data
        .split(Array('\n', ' '))
        .map(_.split(":"))
        .map { case Array(k, v) => k -> v }.toMap
    }
    input.split("\\n\\n").map(parsePassport).toList
  }

  def isPassportValid(passport: Passport): Boolean = {
    val requiredFields = Set("ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt")
    (requiredFields diff passport.keySet).isEmpty
  }

  val input = Using(Source.fromURL(getClass.getResource("input-day04.txt")))(_.mkString).get
  val passports: List[Passport] = parsePassports(input)

  val solutionPart1 = passports.count(isPassportValid)
  require(solutionPart1 == 250, s"part 1: expected solution 250, got $solutionPart1")
}
