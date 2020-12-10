package aoc2020
import aoc2020.lib._

def day10(ratings: List[Int]): String =
  val adapters = (0 :: (ratings.max + 3) :: ratings).sorted
  val joltages = adapters.sliding(2,1).toSeq
    .groupMapReduce{case List(low, high) => high - low}(_ => 1)(_ + _)

  val variants = adapterVariants(0, adapters.tail)

  s"Joltage differences:\n 1 Jolt: ${joltages(1)}\n 3 Jolts: ${joltages(3)}\n"+
  s" Product: ${joltages(1) * joltages(3)}\n" +
  s"\nPossible variants: ${variants}"




val adapterVariants: ((Int, List[Int])) => Long = memoize{ case (prev, remaining) => {
  remaining match {
    case h :: t if h < prev + 4 => adapterVariants(h, t) + adapterVariants(prev, t)
    case last :: Nil => 1
    case _ => 0
  }
}}
