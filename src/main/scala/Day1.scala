package aoc2020
object day1 extends (List[Int] => String) {
  def apply(input: List[Int]): String =
    List(2, 3).map(groupSize => input.groupWithSum(2020, groupSize).product)
      .mkString("\n")

  extension (input: List[Int]) def groupWithSum(sum: Int, groupSize: Int = 2): List[Int] =
    input.combinations(groupSize).filter(_.sum == sum).next
}
