package aoc2020
@main def runDay(inputDir: String, day: Int): Unit =
  given Location(inputDir, day)
  //given Fixed(sample)
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
    case _ => "No such day implemented"
  }
  println(out)

//val sample: List[String] = List()
