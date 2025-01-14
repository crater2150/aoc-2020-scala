package aoc2020
object day2 extends (List[String] => String) {
  def apply(passwords: List[String]): String =
    List(
      passwords.count(validAmount),
      passwords.count(validPositions)
    ).mkString("\n")


  // part 1, password must contain given amount of character
  def validAmount(passLine: String): Boolean =
    val (min, max, char, pass) = parsePass(passLine)
    (min to max) contains pass.count(_ == char)

  // part 2, password must contain character at exactly one of the given positions
  def validPositions(passLine: String): Boolean =
    val (a, b, char, pass) = parsePass(passLine)
    pass(a-1) == char ^ pass(b-1) == char

  def parsePass(passLine: String): (Int, Int, Char, String) =
    val s"$min-$max $char:$pass" = passLine
    (min.toInt, max.toInt, char(0), pass)
}
