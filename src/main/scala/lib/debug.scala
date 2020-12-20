package aoc2020.lib

import aoc2020.day20._
import Vectors._
import Directions._

object DebugDay20 {
  def printCoordMap(coords: Map[Vec2D[Int], Tile], sideLength: Int): Unit =
      def get(x: Int, y: Int) = coords(Vec2D(x, y))
      def chr(b: Boolean): String = if b then "#" else "."

      println("DEBUG:\n" + 
        (for(y <- 0 until sideLength) yield {
          (0 until sideLength).foldLeft(Vector.fill(10)(Vector[String]()))((cols, x) => {
            val tile = get(x, y).image
            (cols zip tile).map((c, t) => (c :+ f"  ${get(x,y).id}%4d> ") ++ t.map(chr))
          }).map(_.mkString).mkString("\n")
        }).reverse.mkString("\n\n")
        +"\nENDDEBUG"
      )

  def printIdPlacement(coords: Map[Vec2D[Int], Tile], sideLength: Int): Unit =
    println(
      (for (x <- 0 until sideLength) yield
        (for (y <- 0 until sideLength) yield coords.get(Vec2D(x,y)).map(_.id).getOrElse(0))
        .mkString(" ")
      ).mkString("\n")
    )

}
