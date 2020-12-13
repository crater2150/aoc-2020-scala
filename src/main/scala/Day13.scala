package aoc2020
import aoc2020.lib._
import cats.implicits.given
import scala.math.{min,max}
import cats._, cats.implicits.given

def day13(input: List[String]): String =
  input match {
    case timestamp :: busses :: Nil =>
      val idsWithOffsets = busses.splitNN(",")
        .zipWithIndex
        .filter(_._1 != "x")
        .map { (id, offset) => (id.toLong, offset.toLong) }

      val (next, wait) = findEarliest(timestamp.toLong, idsWithOffsets.map(_._1))

      val contestTimestamp = findContinuousDepartures(idsWithOffsets)
      s"Next bus: $next in $wait minutes (solution: ${next * wait})\nPlanning Contest timestamp: $contestTimestamp"

    case _ => "Expected two lines of input"
  }

def findEarliest(timestamp: Long, departures: List[Long]): (Long, Long) =
  departures.map(id => (id, id - timestamp % id)).minBy(_._2)

import scala.math.BigInt
def findContinuousDepartures(idsWithOffsets: List[(Long,Long)]): BigInt =

  // naive apporach, much too slow
  //val first :: later = idsWithOffsets
  //LazyList.iterate(0L)(_ + first).find(start => {
  //    later.forall((id, offset) => (start + offset) % id == 0)
  //  }).get
 
  /* using https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Generalization_to_non-coprime_moduli
   * the solution can be found in one iteration */
  val mods = idsWithOffsets.map((id, off) => (BigInt(id), BigInt((id - off) % id)))

  mods.tail.foldLeft(mods.head) { case ((mod, time), (nmod, offset)) => 
    val (_, s, t) = extendedEuclid(mod, nmod)
    val modprod = mod * nmod
    val ntime = (time * s * nmod + offset * t * mod).mod(modprod)
    (modprod, ntime)
  }._2

/** extended euclidian algorithm
 *  https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
 *  @return (gcd, Bézout coeffients s, t) */
def extendedEuclid(a: BigInt, b: BigInt): (BigInt,BigInt,BigInt) =
  @annotation.tailrec
  def rec(r: BigInt, rp: BigInt, s: BigInt, sp: BigInt, t: BigInt, tp: BigInt): (BigInt, BigInt, BigInt) =
    val q = rp / r
    val rn = rp - r * q
    val sn = sp - s * q
    val tn = tp - t * q
    if rn == 0 then (r, s, t)
    else rec(rn, r, sn, s, tn, t)
  rec(a, b, 0, 1, 1, 0)

def lcm(nums: Vector[Long]): Long =
  def rec(mi: Vector[Long]): Long =
    val (m, i) = mi.zipWithIndex.min
    val miNext = mi.updated(i, m + nums(i))
    if miNext.distinct.size == 1 then miNext.head
    else rec(miNext)
  rec(nums)
