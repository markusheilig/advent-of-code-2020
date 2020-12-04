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

  object part1 {
    def isPassportValid(passport: Passport): Boolean = {
      val requiredFields = Set("ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt")
      (requiredFields diff passport.keySet).isEmpty
    }
  }

  object part2 {
    def isPassportValid(passport: Passport): Boolean = {

      def validate(key: String, fn: String => Boolean): Boolean = passport.get(key).exists(fn)
      def isBetween(min: Int, max: Int)(number: String): Boolean = number.toIntOption.exists(x => min <= x && x <= max)

      validate("byr", isBetween(1920, 2002)) &&
        validate("iyr", isBetween(2010, 2020)) &&
        validate("eyr", isBetween(2020, 2030)) &&
        passport.get("hgt").exists {
          case s"${cm}cm" => isBetween(150, 193)(cm)
          case s"${in}in" => isBetween(59, 76)(in)
          case _          => false
        } &&
        validate("hcl", "#[0-9a-f]{6}".r.matches) &&
        validate("ecl", "amb|blu|brn|gry|grn|hzl|oth".r.matches) &&
        validate("pid", "[0-9]{9}".r.matches)
    }
  }

  val input = Using(Source.fromURL(getClass.getResource("input-day04.txt")))(_.mkString).get
  val passports: List[Passport] = parsePassports(input)

  val solutionPart1 = passports.count(part1.isPassportValid)
  require(solutionPart1 == 250, s"part 1: expected solution 250, got $solutionPart1")

  val solutionPart2 = passports.count(part2.isPassportValid)
  require(solutionPart2 == 158, s"part 2: expected solution 158, got $solutionPart2")
}
