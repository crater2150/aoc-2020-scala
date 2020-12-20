package aoc2020
import aoc2020.lib._

object day19 extends (List[String] => String) {
  type Rules = Map[Int, Rule]
  import Rule._
  import Optimizer._

  def apply(input: List[String]): String =
    val (rules, _ :: messages) = input.span(_ != "")
    given ruleset as Rules = rules
      .map{ case s"$index: $rule" => (index.toInt, parse(index, rule)) }
      .toMap
    val opt = ruleset.optimize.prune(0)

    val alternateRuleset = ruleset
      .updated(8, parse("8L", "42 | 42 8"))
      .updated(11, parse("11L", "42 31 | 42 11 31"))

    val optAlt = alternateRuleset.optimize.prune(0)
    val rootRule = opt(0)
    val rootRuleAlt = optAlt(0)
    s"""
       |${opt.format("Basic rules (pruned)", false)}
       |${optAlt.format("Recursive rules (pruned)", false)}
       |${messages.count(msg => rootRule.fullyMatches(msg)(using opt))}
       |${messages.count(msg => rootRuleAlt.fullyMatches(msg)(using optAlt))}
       |""".stripMargin

  sealed trait Rule:
    def matches(message: String, nest: Int = 0)(using ruleset: Rules): List[(Boolean, String)]

    def fullyMatches(message: String)(using ruleset: Rules): Boolean =
      matches(message).contains((true, ""))

    def toExpr: String = this match
      case Fixed(_, content)   => s""" "$content" """
      case Concat(_, rules)    => 
        val expr = rules.map{
          case r: Rule => r.toExpr
          case i: Int => i.toString
        }.mkString(" ~ ")
        s"($expr)"
      case Alternate(_, rules) => s"(${rules.map(_.toExpr).mkString(" | ")})"

  object Rule:
    def parse(name: String, rule: String): Rule =
      rule.trim.nn match {
        case s"$left | $right" => Alternate(name, List(parse(name + "-left", left), parse(name + "-right", right)))
        case c if c.head == '"' => Fixed(name, c(1).toString)
        case subrules => Concat(name, subrules.split(" ").nn.map(_.toInt).toList)
      }

  case class Fixed(name: String, content: String) extends Rule:
    def matches(message: String, nest: Int = 0)(using ruleset: Rules): List[(Boolean, String)] =
      if message.startsWith(content) then List((true, message.drop(content.length)))
      else List((false, message))

    override def toString: String = s"""("$content" @$name)"""

  case class Concat(name: String, rules: List[Rule|Int]) extends Rule:
    def matches(message: String, nest: Int = 0)(using ruleset: Rules): List[(Boolean, String)] =
      rules.map(deref(ruleset)).foldLeft(List((true, message))){
        case (List((true, "")), _) => List((false, ""))
        case (prev, rule) => prev.flatMap((_, rest) => rule.matches(rest, nest+1)).filter(_._1)
      }

    override def toString: String = s"(${rules.mkString(" ~ ")} @$name)"

  case class Alternate(name: String, alternatives: List[Rule]) extends Rule:
    def matches(message: String, nest: Int = 0)(using ruleset: Rules): List[(Boolean, String)] =
      if(message == "") List()
      else
        alternatives.collect(((rule: Rule) => {
          val test = rule.matches(message, nest+1).filter(_._1)
          if test.nonEmpty then Some(test) else None
        }).unlift).flatten

    override def toString: String = s"(${alternatives.mkString(" | ")} @$name)"

  def deref(rules: Rules)(key: Rule | Int): Rule = key match {
    case r: Rule => r
    case i: Int => rules(i)
  }

  extension (rules: Rules)
    def format(name: String = "Ruleset", labeled: Boolean = true): String = 
      rules.toList.sortBy(_._1).map((i, r) => f"${i}%3d: ${if labeled then r.toString else r.toExpr}")
        .mkString(s"$name:\n - ", "\n - ", "")

  /* probably totally superfluous, but made interpreting the rules simpler */
  object Optimizer {
      def AlternateOpt(name: String, alternatives: List[Rule]): Alternate =
        Alternate(name, alternatives.distinct)

      def optimizeSingle(ruleset: Rules)(rule: Rule):Rule =
        rule match
          case Concat(name, List(n)) => deref(ruleset)(n)
          case keep as Concat(_, _) =>
            optimizeConcat(ruleset)(keep)
          case keep as Alternate(name, rules) =>
            val optRules = rules.map(optimizeSingle(ruleset)).distinct.flatMap {
              case Alternate(_, alts) => alts
              case keep => List(keep)
            }
            optRules match {
              case r :: Nil => r
              case opt => AlternateOpt(name, opt)
            }
          case keep => keep

      def optimizeConcat(ruleset: Rules)(orig: Concat, nameSuffix: String = ""): Rule =
        val subrules = orig.rules.map(deref(ruleset))

        def rec(rules: List[Rule], newRules: List[Rule]): List[Rule] =
          rules match {
            case (fix as Fixed(_, s)) :: Alternate(altName, alts) :: rest =>
              val merged = AlternateOpt(altName+"-opt-subsume"+nameSuffix, alts.map {
                case Concat(n, con) => Concat(n+"-joined", fix :: con)
                case Fixed(_, c) => Fixed("joined"+nameSuffix, s + c)
                case a as Alternate(_,_) => Concat(altName+"-joined?", List(fix, a))
              })
              rec(merged :: rest, newRules)
            case Fixed(sn,s) :: Fixed(tn, t) :: rest =>
              val merged = Fixed(s"$sn+$tn", s + t)
              rec(merged :: rest, newRules)
            case Alternate(altName, alts) :: (fix as Fixed(_, s)) :: rest =>
              val merged = AlternateOpt(altName+"-opt-subsume"+nameSuffix, alts.map {
                case Concat(n, con) => Concat(n+"-joined", con :+ fix)
                case Fixed(_, c) => Fixed("joined"+nameSuffix, c + s)
                case a as Alternate(_,_) => Concat(altName+"-joined?", List(a, fix))
              })
              rec(merged :: rest, newRules)
            case h :: tail => rec(tail, h :: newRules)
            case Nil => newRules.reverse
          }

        Concat(orig.name, rec(subrules, List()))

      extension (self: Rule)
        def references(ref: Int): Boolean = self match {
          case Concat(_, refs) => refs.exists {
            case r: Rule => r.references(ref)
            case i: Int => ref == i
          }
            case Alternate(_, rules) => rules.exists(_.references(ref))
            case _ => false
        }

      extension (rules: Rules)
        def optimize: Rules =
          val opt = rules.mapValuesS(Optimizer.optimizeSingle(rules))
          if opt == rules then opt else opt.optimize

        def prune(root: Int = 0): Rules =
          rules.filter((key, rule) => key == root || rules.exists(_._2.references(key)))
  }
}
