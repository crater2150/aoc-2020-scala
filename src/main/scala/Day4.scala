import aoc2020.lib.Passports._

def day4(input: List[String]): String =
  val complete = findPassports(input).filter(_.hasRequiredFields)
  s"complete: ${complete.length}\nvalid:    ${complete.count(_.isValid)}"

def findPassports(input: List[String]): List[Passport] =
  def go(lines: List[String], accu: List[Passport]): List[Passport] =
    lines.span(_ != "") match {
      case (passport, "" :: rest) => go(rest, Passport.fromStrings(passport) :: accu)
      case (passport, Nil) => Passport.fromStrings(passport) :: accu
      case _ => List() // malformed input
    }
  go(input, List())



