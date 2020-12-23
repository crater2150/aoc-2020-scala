package aoc2020
import aoc2020.lib._
import cats._, cats.implicits.given

object day23 extends (Vector[Int] => String) {
  def apply(input: Vector[Int]): String =
    val testRun = playGameVec(input, 100)
    val realGame = playGameVec(input ++ (10 to 1_000_000), 10_000_000)
    val (n1, n2) = pairAfter1(realGame)
    s"""Test run labels: ${finalLabels(testRun, input.size)}\n
        |Full run labels next to 1: $n1 * $n2 = ${n1 * n2}
        |""".stripMargin

  def finalLabels(cups: Int => Int, size: Int): String =
    toSolutionVector(cups, 1, size).tail.mkString

  def pairAfter1(cups: Int => Int): (Long, Long) =
    (cups(1).toLong, cups(cups(1)).toLong)

  def playGameVec(cups: Vector[Int], moves: Int = 100): Int => Int =
    val permutation = 0 +: cups.zip(cups.tail :+ cups.head).sortBy(_._1).map(_._2)
    (1 to moves).foldLeft((cups.head, permutation))((c, _) => moveVecT(c))._2

  val moveVecT = moveVec.tupled
  def moveVec(current: Int, cups: Vector[Int]): (Int, Vector[Int]) =
    val next = LazyList.iterate(current)(cups).tail.take(4)
    val removed = next.init
    val target = (1 to 4).map(i => if current - i < 1 then current - i + cups.size - 1 else current - i)
                         .filterNot(removed.contains).head

    val newCups = cups
      .updated(current, next.last)
      .updated(target, removed.head)
      .updated(removed.last, cups(target))
    (next.last, newCups)

  def toSolutionVector(map: Int => Int, start: Int, size: Int): Vector[Int] =
    (2 to size).scanLeft(start)((prev, _) => map(prev)).toVector

  /* This is factor 3 slower than using the Vector solution above */
  def playGame(cups: Vector[Int], moves: Int = 100): Map[Int,Int] =
    val mappedCups = cups.zip(cups.tail :+ cups.head).toMap
    (1 to moves).foldLeft((cups.head, mappedCups))((c, _) => moveT(c))._2

  val moveT = move.tupled
  def move(current: Int, cups: Map[Int, Int]): (Int, Map[Int, Int]) =
    val next = LazyList.iterate(current)(cups).tail.take(4)
    val removed = next.init
    val target = (1 to 4).map(i => if current - i  < 1 then current - i + cups.size else current - i)
                         .filterNot(removed.contains).head

    val newCups = cups ++ Map(
      current -> next.last,
      target -> removed.head, 
      removed.last -> cups(target)
    )
    (next.last, newCups)

}
