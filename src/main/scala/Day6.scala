package aoc2020
import aoc2020.lib._

object day6 extends (List[String] => String) {
  def apply(input: List[String]): String =
    //countGroupAnswersAny(input).sum.toString
    countGroupAnswersAll(input).sum.toString

  def countGroupAnswersAny(input: List[String]): LazyList[Int] =
    input.split("").map(_.flatMap(_.toArray).toSet.size)

  def countGroupAnswersAll(input: List[String]): LazyList[Int] =
    input.split("").map(lines => {
      val answers = lines.map(_.toSet)
      answers.headOption
        .map(h => answers.tail.fold(h)(_ intersect _))
        .map(_.size)
        .getOrElse(0)
    })
}
