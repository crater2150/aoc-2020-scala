
import scala.io.Source

case class Location(inputDir: String, day: Int)

def inputLines(using l: Location) = scala.io.Source.fromFile(s"${l.inputDir}/day${l.day}.txt").getLines

def input[A](format: String => A = identity)(solver: List[A] => String)(using l: Location): String =
  solver(inputLines.map(format).toList)

def boolChar(trueChar: Char): String => Vector[Boolean] = _.map(_ == trueChar).toVector
