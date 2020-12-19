package aoc2020

import aoc2020.lib._

def day15(input: List[String]): String =
  val starting: Map[Long, Long] = parse(input.head)
  val (num2020, _) = playMut(starting, 2020)
  val (num30mil, _) = playMut(starting, 30000000)
  s"""2020th spoken number: $num2020
     |30 millionth spoken number: $num30mil
     """.stripMargin

def parse(input: String): Map[Long, Long] =
    input.split(",").nn
      .zipWithIndex
      .map(t => (t._1.toLong, t._2 + 1L))
      .toMap

def play(starting: Map[Long, Long], maxTurns: Long) =
  val (lastNum, lastTurn) = starting.maxBy(_._2)

  (lastTurn to maxTurns-1).foldLeft((lastNum, starting - lastNum)){
    case ((last, spoken), turn) =>
      val next = spoken.get(last).map(turn - _).getOrElse(0L)
      val nowSpoken = spoken.updated(last, turn)
      (next, nowSpoken)
  }

/* about twice as fast for a single run */
import scala.collection.mutable
def playMut(starting: Map[Long, Long], maxTurns: Long) =
  val (lastNum, lastTurn) = starting.maxBy(_._2)
  val spoken = mutable.Map.from(starting - lastNum)
  var last: Long = lastNum
  for (turn <- (lastTurn to maxTurns-1)) {
      val next = spoken.get(last).map(turn - _).getOrElse(0L)
      spoken += last -> turn
      last = next
  }
  (last, spoken)
