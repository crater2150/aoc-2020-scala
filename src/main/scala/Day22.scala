package aoc2020
import aoc2020.lib._
import cats._, cats.implicits.given

object day22 extends (List[String] => String) {
  def apply(input: List[String]): String =
    val (p1input, p2input) = input.span(_ != "")
    val p1deck = p1input.tail.view.map(_.toInt).toVector
    val p2deck = p2input.drop(2).view.map(_.toInt).toVector

    val winnerDeck = play(p1deck, p2deck)
    val winnerScore = score(winnerDeck)
    val (_, winnerDeckRec) = recCombat(p1deck, p2deck)
    val winnerScoreRec = score(winnerDeckRec)
    s"Score: $winnerScore\nRecScore: $winnerScoreRec"

  def score(deck: Vector[Int]): Int =
    deck.zipWithIndex.map((card, i) => card * (deck.size - i)).sum

  @annotation.tailrec
  def play(p1deck: Vector[Int], p2deck: Vector[Int]): Vector[Int] =
    if p1deck.isEmpty then p2deck
    else if p2deck.isEmpty then p1deck
    else if p1deck.head > p2deck.head then
      play(p1deck.tail :+ p1deck.head :+ p2deck.head, p2deck.tail)
    else
      play(p1deck.tail, p2deck.tail :+ p2deck.head :+ p1deck.head)

  def recCombat(p1deck: Vector[Int], p2deck: Vector[Int], prevStates: Set[(Vector[Int],Vector[Int])] = Set()): (Boolean, Vector[Int]) = 
    if prevStates((p1deck, p2deck)) then (true, p1deck)
    else if p1deck.isEmpty then (false, p2deck)
    else if p2deck.isEmpty then (true, p1deck)
    else
      val (p1card, p2card) = (p1deck.head, p2deck.head)
      val p1wins =
        if p1card <= p1deck.length - 1 && p2card <= p2deck.length - 1 then
          recCombat(p1deck.tail.take(p1card), p2deck.tail.take(p2card))._1
        else p1card > p2card
      if p1wins then
        recCombat(p1deck.tail :+ p1deck.head :+ p2deck.head, p2deck.tail,
          prevStates + ((p1deck, p2deck)))
      else
        recCombat(p1deck.tail, p2deck.tail :+ p2deck.head :+ p1deck.head, 
          prevStates + ((p1deck, p2deck)))

}
