import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day11 extends App {

  type Position = (Int, Int)
  type Seat = Char
  val floor: Seat = '.'
  val empty: Seat = 'L'
  val occupied: Seat = '#'

  case class Seats(allotment: Map[Position, Seat], width: Int, height: Int)

  def iterate(seats: Seats): Seats = {
    val newSeats = seats.allotment.map { case (position, seat) =>
      val newSeat =
        if (seat == floor) floor
        else adjacentSeats(seats, position) match {
          case adj if seat == empty && !adj.contains(occupied) => occupied
          case adj if seat == occupied && adj.count(_ == occupied) >= 4 => empty
          case _ => seat
        }
      position -> newSeat
    }
    seats.copy(allotment = newSeats)
  }

  def adjacentSeats(seats: Seats, position: Position): Seq[Seat] =
    for {
      dx <- (-1 to 1).toList
      dy <- (-1 to 1).toList
      if !(dx == 0 && dy == 0)
      seat <- seats.allotment.get((dx + position._1, dy + position._2))
    } yield seat

  def parse(input: String): Seats = {
    val lines = input.split("\\n").toList
    val allotment = for {
      (line, y) <- lines.zipWithIndex
      (seat, x) <- line.zipWithIndex
    } yield (x, y) -> seat
    Seats(allotment.toMap, width = lines.head.length, height = lines.size)
  }

  @tailrec
  def solve1(seats: Seats): Int = {
    val next = iterate(seats)
    if (next == seats) seats.allotment.values.count(_ == occupied) else solve1(next)
  }

  val input = Using(Source.fromURL(getClass.getResource("input-day11.txt")))(_.mkString).get
  val seats = parse(input)

  val solutionPart1 = solve1(seats)
  assert(solutionPart1 == 2261, s"part 1: expected solution 2261, got $solutionPart1")
}
