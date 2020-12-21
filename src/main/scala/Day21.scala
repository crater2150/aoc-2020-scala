package aoc2020

import aoc2020.lib._
import cats._, cats.implicits.given
import scala.collection.immutable.SortedMap

object day21 extends (List[String] => String) {
  def apply(input: List[String]): String =
    val (foods, allergenCandidates, byAllergen) = parse(input)
    val cleanCandidates = allergenCandidates.map((ingr, allergens) =>
        ingr -> allergens.filter(al => byAllergen(al).forall(_.contains(ingr))))
    val nonAllergenic = cleanCandidates.filter(_._2.isEmpty).keySet
    val allergenMap = matchAllergens(cleanCandidates)
    val numberSafe = foods.map(_.count(nonAllergenic)).sum

    s"""Foods: $foods
       |foods by allergen: ${byAllergen.mkString("\n -","\n -","")}
       |clean candidates: ${cleanCandidates.mkString("\n -","\n -","")}
       |Non-allergenic ingredients:
       |${formatColumns(nonAllergenic)}
       |Safe occurrences: ${numberSafe}
       |Actual allergens: ${allergenMap.mkString("\n -","\n -","")}
       |Canonical dangerous ingredients list: ${allergenMap.values.mkString(",")}
       |""".stripMargin

  def formatColumns[T](values: Iterable[T]): String =
    values.map(v => f"$v%-10s").grouped(7).map(_.mkString("- ", "  - ", "")).mkString("\n")

  def matchAllergens(allergensPerIngr: Map[String, Set[String]]): SortedMap[String, String] =
    def rec(candidates: Map[String, Set[String]], matched: Map[String, String]): Map[String, String] =
      candidates.find(_._2.size == 1) match {
        case None => matched
        case Some((ingr, allergen)) =>
          val remaining = candidates.view.mapValues(_ - allergen.head).filter(_._2.nonEmpty).toMap
          rec(remaining, matched + (ingr -> allergen.head))
      }
    SortedMap.from(rec(allergensPerIngr.filter(_._2.nonEmpty), Map()).map(_.swap))


  def parse(input: List[String]): (List[Set[String]], Map[String, Set[String]], Map[String, List[Set[String]]]) =
    val rawData = input.map { case s"$ingredients (contains $allergens)" =>
        (ingredients.split(" ").nn.toSet, allergens.split(", ").nn.toList)}

    val ingredientsPerFood = rawData.map(_._1)
    val allergenCandidates = ( for (ingredients, allergens) <- rawData
                                   ingredient               <- ingredients
                               yield Map(ingredient -> allergens.toSet)
                             ).combineAll
    val foodsByAllergen = rawData.flatMap(entry => entry._2.map(allergen => Map(allergen -> List(entry._1)))).combineAll

    (ingredientsPerFood, allergenCandidates, foodsByAllergen)
}
