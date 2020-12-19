package aoc2020
import lib.RingBuffer

object day9 extends (List[Long] => String) {
  def apply(input: List[Long]): String =
    (
      for {
      invalid <- findInvalidInInput(input, 25)
      range <- findSumRange(input, invalid)
      } yield (invalid, range)
    ).fold("No invalid number"){
      case (invalid, (min, max)) =>
        s"first invalid: $invalid\nMin/max of sum range: $min / $max\nWeakness (min + max): ${min + max}"
    }


  def findInvalidInInput(input: List[Long], preambleSize: Int) =
    val (preamble, message) = input.splitAt(preambleSize)
    findInvalid(RingBuffer(preamble.toVector, preambleSize), message)


  def findInvalid(preamble: RingBuffer[Long], message: List[Long]): Option[Long] =
    message match {
      case h :: t =>
        if (preamble.combinations(2).exists(_.sum == h)) findInvalid(preamble :+ h, t)
        else Some(h)
      case Nil => None
    }

  def findSumRange(input: List[Long], invalid: Long) =
    import math.{min, max}

    @annotation.tailrec
    def checkHead(remaining: List[Long], sum: Long, minS: Long, maxS: Long): Option[(Long, Long)] =
      remaining match {
        case Nil => None
        case h :: t =>
          val nsum = sum + h
          if (nsum > invalid) None
          else if (nsum == invalid) Some((min(minS, h), max(maxS, h)))
          else checkHead(t, nsum, min(minS, h), max(maxS, h))
      }

    input.tails.collectFirst((checkHead(_, 0L, Long.MaxValue, Long.MinValue)).unlift)

}
