import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day11 extends App {

  type Position = (Int, Int)
  type Seat = Char
  type FindAdjacents = (Seats, Position) => List[Seat]
  val floor: Seat = '.'
  val empty: Seat = 'L'
  val occupied: Seat = '#'
  val directions = for {
    x <- (-1 to 1).toList
    y <- (-1 to 1).toList
    if !(x == 0 && y == 0)
  } yield (x, y)

  case class Seats(allotment: Map[Position, Seat], width: Int, height: Int)

  def iterate(seats: Seats, findAdjacents: FindAdjacents, occupancyTolerance: Int): Seats = {
    val newSeats = seats.allotment.map { case (position, seat) =>
      val newSeat =
        if (seat == floor) floor
        else findAdjacents(seats, position) match {
          case adj if seat == empty && !adj.contains(occupied) => occupied
          case adj if seat == occupied && adj.count(_ == occupied) >= occupancyTolerance => empty
          case _ => seat
        }
      position -> newSeat
    }
    seats.copy(allotment = newSeats)
  }

  def adjacentSeatsPart1(seats: Seats, position: Position): List[Seat] =
    for {
      (x, y) <- directions
      seat <- seats.allotment.get((x + position._1, y + position._2))
    } yield seat

  def adjacencySeatsPart2(seats: Seats, position: Position): List[Seat] = {
    @tailrec
    def walk(position: Position, dirX: Int, dirY: Int): Option[Seat] = {
      val next = (position._1 + dirX, position._2 + dirY)
      seats.allotment.get(next) match {
        case None             => None
        case Some(`occupied`) => Some(occupied)
        case Some(`empty`)    => Some(empty)
        case Some(`floor`)    => walk(position = next, dirX, dirY)
      }
    }

    for {
      (x, y) <- directions
      seat <- walk(position, x, y)
    } yield seat
  }

  def parse(input: String): Seats = {
    val lines = input.split("\\n").toList
    val allotment = for {
      (line, y) <- lines.zipWithIndex
      (seat, x) <- line.zipWithIndex
    } yield (x, y) -> seat
    Seats(allotment.toMap, width = lines.head.length, height = lines.size)
  }

  @tailrec
  def solve(seats: Seats, findAdjacents: FindAdjacents, occupancyTolerance: Int): Int = {
    val next = iterate(seats, findAdjacents, occupancyTolerance)
    if (next == seats) seats.allotment.values.count(_ == occupied) else solve(next, findAdjacents, occupancyTolerance)
  }

  val input = Using(Source.fromURL(getClass.getResource("input-day11.txt")))(_.mkString).get
  val seats = parse(input)

  val solutionPart1 = solve(seats, adjacentSeatsPart1, occupancyTolerance = 4)
  assert(solutionPart1 == 2261, s"part 1: expected solution 2261, got $solutionPart1")

  val solutionPart2 = solve(seats, adjacencySeatsPart2, occupancyTolerance = 5)
  assert(solutionPart2 == 2039, s"part 2: expected solution 2039, got $solutionPart2")
}
