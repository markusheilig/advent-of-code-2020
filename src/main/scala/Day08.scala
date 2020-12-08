import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day08 extends App {

  type Instruction = (String, Int)
  type Program = List[Instruction]

  def parseProgram(input: String): Program = {
    val instructionPattern = """([a-z]{3}) (-[0-9]+|\+[0-9]+)""".r
    input.split("\\n").toList.map { instruction =>
      val instructionPattern(operation, argument) = instruction
      (operation, argument.toInt)
    }
  }

  case class ProgramExit(result: Int, aborted: Boolean)

  def run(program: Program): ProgramExit = {
    @tailrec
    def run(pc: Int, acc: Int, seen: Set[Int]): ProgramExit = {
      if (seen.contains(pc)) ProgramExit(acc, aborted =  true)
      else program.lift(pc) match {
        case Some(("nop", _))   => run(pc + 1, acc, seen + pc)
        case Some(("jmp", arg)) => run(pc + arg, acc, seen + pc)
        case Some(("acc", arg)) => run(pc + 1, acc + arg, seen + pc)
        case None => ProgramExit(acc, aborted = false)
      }
    }
    run(0, 0, Set.empty)
  }

  def fix(program: Program): Program = {
    @tailrec
    def repeatUntilFixed(index: Int = 0): Program = {
      val Some((operation, i)) = program.zipWithIndex.collectFirst {
        case (("nop", arg), i) if i >= index => (("jmp", arg), i)
        case (("jmp", arg), i) if i >= index => (("nop", arg), i)
      }
      val fixedProgram = program.updated(i, operation)
      val programExit = run(fixedProgram)
      if (programExit.aborted) repeatUntilFixed(i + 1) else fixedProgram
    }
    repeatUntilFixed()
  }

  val input = Using(Source.fromURL(getClass.getResource("input-day08.txt")))(_.mkString).get
  val program = parseProgram(input)

  val solutionPart1 = run(program).result
  assert(solutionPart1 == 1814, s"part 1: expected solution 1814, got $solutionPart1")

  val solutionPart2 = run(fix(program)).result
  assert(solutionPart2 == 1056, s"part 2: expected solution 1056, got $solutionPart2")
}
