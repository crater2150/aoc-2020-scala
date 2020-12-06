package aoc2020
import aoc2020.lib._
import aoc2020.lib.Passports._

def day4(input: List[String]): String =
  val complete = findPassports(input).filter(_.hasRequiredFields)
  s"complete: ${complete.length}\nvalid:    ${complete.count(_.isValid)}"

def findPassports(input: List[String]): List[Passport] =
  input.split("").foldLeft(List[Passport]())(
    (passes, lines) => Passport.fromStrings(lines) :: passes
  )
