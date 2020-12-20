package aoc2020
import aoc2020.lib._

def day16(input: List[String]): String =
  val List(rulesIn, List(_, ownIn), _ :: nearbyIn) = input.split("").toList
  val rules = parseRules(rulesIn)
  val own = parseTicket(ownIn)
  val nearby = nearbyIn.map(parseTicket)

  val (errorRate, valid) = nearby.foldLeft((0, Vector[Vector[Int]]())){
    case ((errors, valids), ticket) => findErrors(rules)(ticket) match {
      case Nil => (errors, valids :+ ticket)
      case err => (errors + err.sum, valids)
    }}
  val columns = findFields(rules)(valid)
  val departureInfo = columns.filter(_._1.startsWith("departure")).mapValuesS(own(_))
  val checksum = departureInfo.values.map(_.toLong).product

  s"""Error rate: $errorRate\n
     |Departure Columns: ${departureInfo.mkString("\n -", "\n -", "")}
     |Departure Checksum: $checksum
     """.stripMargin

type Rules = Map[String, Vector[Range]]

def parseRules(input: List[String]): Rules =
  input.map {
    case s"$name: $ranges" => name -> ranges.split(" or ").nn.map(parseRange).toVector
  }.toMap

def parseRange(str: String): Range = str.splitOnce("-").map(_.toInt to _.toInt).get
def parseTicket(str: String): Vector[Int] = str.split(",").nn.map(_.toInt).toVector

def findErrors(rules: Rules)(ticket: Vector[Int]): Vector[Int] =
  ticket.filter(num => !rules.values.flatten.exists(_ contains num))

def findFields(rules: Rules)(tickets: Vector[Vector[Int]]) =
  val candidates = tickets.transpose.map(col => rules.filter((rule, ranges) =>
          col.forall(c => ranges.exists(_ contains c))
      ).keys.toSet)
  candidates.zipWithIndex.sortBy(_._1.size).foldLeft(Map[String, Int]()){
    case (legend, (candidates, col)) => legend + ((candidates -- legend.keys).head -> col)
  }
