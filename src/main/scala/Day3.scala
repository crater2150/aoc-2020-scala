package aoc2020
object day3 extends (List[Vector[Boolean]] => String) {
  def apply(trees: List[Vector[Boolean]]): String =
    treesOnSlope(trees, 3).toString + "\n" + List(
      treesOnSlopeRec(trees, 1, 1),
      treesOnSlopeRec(trees, 3, 1),
      treesOnSlopeRec(trees, 5, 1),
      treesOnSlopeRec(trees, 7, 1),
      treesOnSlopeRec(trees, 1, 2),
      ).product.toString


  /* Solution with both right and down step as parameter */
  def treesOnSlopeRec(treeMap: List[Vector[Boolean]], rightSlope: Int, downSlope: Int): Long =
    @annotation.tailrec
    def go (trees: List[Vector[Boolean]], r: Int, hits: Long): Long =
      val rest = trees.drop(downSlope)
      rest match {
        case line :: _ => go(rest, r + rightSlope, if line(r % line.size) then hits + 1 else hits)
        case Nil => hits
      }
    go(treeMap, rightSlope, 0)

  /* Solution for the first part.
   * Can also be used for most of part 2, except the one with a down step != 1,
   * a different approach was simpler than changing this to accomodate for it
   */
  def treesOnSlope(trees: List[Vector[Boolean]], rightSlope: Int): Int =
    val steps = rightSlope to rightSlope * trees.size by rightSlope
    (
      for (i, line) <- steps.zip(trees.drop(1))
      yield line(i % line.size)
    ).count(identity)
}
