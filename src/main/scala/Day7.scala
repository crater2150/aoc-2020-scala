package aoc2020
import lib._
import cats._
import cats.implicits.given

def day7(input: List[String]):String =
  val rules = parseRules(input)
  //findPossibleSuperbags(rules, "shiny gold").size.toString
  subBags(rules, "shiny gold").toString

def parseRules(input: List[String]): BagRules =
  input.map(parseSingle).combineAll

val RuleFormat = raw"(.*) bags contain (?:(no other)|(.*)) bags?.".r

def parseSingle(rule: String): BagRules = 
  val RuleFormat(outerColor, empty, contents) = rule
  val forward = if(empty == "no other") List() else parseContents(contents)
  val reverse = (
      for (innerColor, count) <- forward
      yield innerColor -> Set(outerColor)
    ).toMap
  BagRules(Map(outerColor -> forward), reverse)


def parseContents(bags: String): List[(String, Int)] = 
  bags.split(" bags?, ").nn.map ( bagSpec => {
    val List(amount, color) = bagSpec.split(" ", 2).nn
    (color, amount.toInt)
  })

case class BagRules(contents: Map[String, List[(String, Int)]], reverse: Map[String, Set[String]])
object BagRules {
  given BagRulesMonoid as Monoid[BagRules] {
    // Members declared in cats.kernel.Monoid    
    def empty: BagRules = BagRules(Map(), Map())

    // Members declared in cats.kernel.Semigroup 
    def combine(x: BagRules, y: BagRules): BagRules =
      BagRules(x.contents |+| y.contents, x.reverse |+| y.reverse)
  }
}

def findPossibleSuperbags(rules: BagRules, color: String): Set[String] =
  def go(superbags: Set[String]): Set[String] =
    val unvisited = superbags.flatMap(c => rules.reverse.getOrElse(c, Set())) -- superbags
    if (unvisited.nonEmpty)
      go(unvisited) ++ superbags
    else
      superbags
  go(rules.reverse(color))

def subBags(rules: BagRules, color: String) = subBagsMem(rules, color) - 1

val subBagsMem: ((BagRules, String)) => Int = memoize {
  case (rules, color) => rules.contents(color).map {
    case (subbag, amount) =>  subBagsMem((rules, subbag)) * amount
  }.sum + 1
}


