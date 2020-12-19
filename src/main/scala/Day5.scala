package aoc2020
import aoc2020.lib._

object day5 extends (List[String] => String) {
  def apply(input: List[String]): String =
    val ids = for i <- input
              yield seatId(seatCoords(i))

    s"max id: ${ids.max}\nown id: ${findOwn(ids).map(_.toString).getOrElse("No free seat!")}"

  def seatCoords(code: String): (Int, Int) =
    (
      recursiveSeat('B', 'F')(code.substring(0,7).nn.toList, 0, 127),
      recursiveSeat('R', 'L')(code.substring(7).nn.toList, 0, 7)
    )


  @annotation.tailrec
  def recursiveSeat(upChar: Char, downChar: Char)(code: List[Char], low: Int, high: Int): Int =
    code match {
      case c :: rest =>
        val (nLow, nHigh) = if c == upChar then up(low, high) else down(low, high)
        recursiveSeat(upChar, downChar)(rest, nLow, nHigh)
      case Nil => low
    }

  inline def up(low: Int, high: Int) = (high - (high - low) / 2, high)
  inline def down(low: Int, high: Int) = (low, high - (high - low) / 2 - 1)
  inline def seatId(loc: (Int, Int)) = loc._1 * 8 + loc._2

  def findOwn(ids: List[Int]): Option[Int] =
    ids.sorted.sliding(2).find{l => l(1) - l(0) == 2}.map(_(0)+1)
}
