package aoc2020
import aoc2020.lib._
import cats._, cats.implicits.given
import Vectors._

object day24 extends (List[String] => String) {
  def apply(input: List[String]): String =
    val directions = input.map(parseLine)
    
    val flipped = directions.map(dirsToCoord)
      .groupBy(identity)
      .filter(_._2.size % 2 == 1)
      .map(_._1).toSet
    val after100days = (1 to 100).foldLeft(flipped)((tiles, _) => gameOfLife(tiles))
    s"""Black tiles on first day: ${flipped.size}
       |Black tiles on first day: ${after100days.size}
       |""".stripMargin

  val dirPattern = raw"(se|sw|nw|ne|e|w)".r
  def parseLine(in: String): List[String] = 
    dirPattern.findAllMatchIn(in).map(_.matched).toList

  
  /* A great article on hexagonal coordinate systems: 
   * https://www.redblobgames.com/grids/hexagons/ 
   * I use an axial coordinate system here */

  val mapVecs = Map(
    "se" -> Vec2D( 0, 1),
    "sw" -> Vec2D(-1, 1),
    "ne" -> Vec2D( 1,-1),
    "nw" -> Vec2D( 0,-1),
    "e"  -> Vec2D( 1, 0),
    "w"  -> Vec2D(-1, 0),
  )

  def dirsToCoord(dirs: List[String]): Vec2D[Int] =
    dirs.foldLeft(Vec2D(0,0)){(pos, dir) => pos + mapVecs(dir)}


  def gameOfLife(blackTiles: Set[Vec2D[Int]], aliveRule: Set[Int] = Set(1,2)): Set[Vec2D[Int]] =
    val neighbours = blackTiles.map(tile => tile -> mapVecs.values.map(_ + tile))
    val stayingBlack = neighbours.filter(aliveRule contains _._2.count(blackTiles)).map(_._1)
    val newBlack = neighbours.flatMap(_._2).filterNot(blackTiles)
                    .filter(whiteTile => mapVecs.values.map(_ + whiteTile).count(blackTiles) == 2)
    stayingBlack ++ newBlack

}
