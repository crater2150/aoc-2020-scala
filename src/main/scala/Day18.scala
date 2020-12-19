package aoc2020
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object day18 extends (List[String] => String) {
  def apply(input: List[String]): String =
    val clean = input.map(ws.replaceAllIn(_, ""))
    val sumNoPrecedence = clean.map(parseExpr(evalNoParen)).sum.toString
    val sumAdditionFirst = clean.map(parseExpr(addThenMult)).sum.toString
    s"Without precedence: $sumNoPrecedence\nWith addition first: $sumAdditionFirst"

  val innerParen: Regex = raw"\(([^()]*)\)".r
  val addition: Regex = raw"(\d+)\+(\d+)".r
  val product: Regex = raw"(\d+)\*(\d+)".r
  val ws = raw"\s".r

  /* basic idea: replace part with highest precedence with its result, until only
   * a number is left */
  @annotation.tailrec
  def parseExpr(eval: String => Long)(expr: String): Long =
    innerParen.replaceFirstWith(expr)(paren => eval(paren.group(1)).toString) match {
      case Some(a) => parseExpr(eval)(a)
      case None => eval(expr)
    }

  /* Part one: evaluate from left to right */
  def evalNoParen(expr: String) =
    @annotation.tailrec
    def rec(result: Long, rest: String): Long =
      if (rest.isEmpty) result
      else
        val (operand, tail) = rest.substring(1).nn.span(_.isDigit)
        rest.head match {
          case '+' => rec(result + operand.toLong, tail)
          case '*' => rec(result * operand.toLong, tail)
        }
    rec(expr.takeWhile(_.isDigit).toLong, expr.dropWhile(_.isDigit))

  /* Part two: additions have higher precedence */
  @annotation.tailrec
  def addThenMult(expr: String): Long =
    @annotation.tailrec
    def mul(in: String): Long =
      product.replaceFirstWith(in)(a => (a.group(1).toLong * a.group(2).toLong).toString) match {
        case Some(a) => mul(a)
        case None => in.toLong
      }
    addition.replaceFirstWith(expr)(a => (a.group(1).toLong + a.group(2).toLong).toString) match {
      case Some(a) => addThenMult(a)
      case None => mul(expr)
    }

  extension (r: Regex)
    def replaceFirstWith(in: String)(replacement: Match => String): Option[String] =
      r.findFirstMatchIn(in)
        .map(mat => in.substring(0, mat.start)
                    + replacement(mat)
                    + in.substring(mat.end))
}
