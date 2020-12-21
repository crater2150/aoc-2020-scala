package aoc2020
import aoc2020.lib._
@main def runDay(inputDir: String, day: Int, sample: Int*): Unit =
  given InputSource = inputSource(inputDir, day, sample.headOption)
  val out = day match {
    case 1 => input(_.toInt)(day1)
    case 2 => input()(day2)
    case 3 => input(boolChar('#'))(day3)
    case 4 => input()(day4)
    case 5 => input()(day5)
    case 6 => input()(day6)
    case 7 => input()(day7)
    case 8 => input()(day8)
    case 9 => input(_.toLong)(day9)
    case 10 => input(_.toInt)(day10)
    case 11 => inputF(boolChar('L'))(Vector)(day11)
    case 12 => input()(day12)
    case 13 => input()(day13)
    case 14 => input()(day14)
    case 15 => input()(day15)
    case 16 => input()(day16)
    case 17 => input(boolChar('#'))(day17)
    case 18 => input()(day18)
    case 19 => input()(day19)
    case 20 => input()(day20)
    case 21 => input()(day21)
    case _ => "No such day implemented"
  }
  if (sample.nonEmpty) println("SAMPLE VALUES!")
  println(out)

def inputSource(inputDir:String, day: Int, sample: Option[Int]) =
  sample.map(SampleLocation(inputDir, day, _))
    .getOrElse(Location(inputDir, day))
