package aoc2020
import aoc2020.lib._

import Vectors._
type Pos = Vec2D[Int]

def day12(input: List[String]): String =
  val directions = input.map(s => (s(0), s.substr(1).toInt))

  val finalPos1 = moveShip(directions)
  val part1 = s"Final position: ${finalPos1.pos}, facing ${finalPos1.dir.str}\nManhattan: ${finalPos1.pos.manhattan}"


  val finalPos2 = moveWaypoint(directions)
  val part2 = s"Final position: ${finalPos2.ship}, waypoint at ${finalPos2.waypoint}\nManhattan: ${finalPos2.ship.manhattan}"
  part1 + "\n\n" + part2

case class Ship(dir: Dir, pos: Pos)

case class WaypointShip(ship: Pos, waypoint: Pos)

def moveShip(directions: List[(Char, Int)]): Ship =
  directions.foldLeft(Ship(East, Vec2D(0,0)))((ship, command) => {
      command match {
        case ('L', deg)  => ship.copy(dir = ship.dir + deg)
        case ('R', deg)  => ship.copy(dir = ship.dir - deg)
        case ('F', dist) => ship.copy(pos = ship.pos.move(ship.dir, dist))
        case (dir, dist) => ship.copy(pos = ship.pos.move(dir.cardinal, dist))
      }
    })


def moveWaypoint(directions: List[(Char, Int)]): WaypointShip =
  directions.foldLeft(WaypointShip(Vec2D(0,0), Vec2D(10,1)))((state, command) => {
      command match {
        case ('L', deg)  => state.copy(waypoint = state.waypoint.rot(deg))
        case ('R', deg)  => state.copy(waypoint = state.waypoint.rot(-deg))
        case ('F', dist) => state.copy(ship = state.ship + state.waypoint * dist)
        case (dir, dist) => state.copy(waypoint = state.waypoint.move(dir.cardinal, dist))
      }
  })


object Vectors {
  import scala.Numeric.Implicits.given

  opaque type Vec2D[T] = (T, T)

  opaque type Dir = Int
  val East: Dir = 0
  val North: Dir = 90
  val West: Dir = 180
  val South: Dir = 270

  extension (c: Char)
    def cardinal: Dir = c match {
      case 'E' => East
      case 'N' => North
      case 'W' => West
      case 'S' => South
    }

  extension (dir: Dir)
    def +(deg: Int): Dir = (dir + deg) % 360
    def -(deg: Int): Dir = ((dir - deg) % 360 + 360) % 360
    def unary_- : Dir = 360 - dir
    def str: String = dir match {
      case East  => "East"
      case North => "North"
      case West  => "West"
      case South => "South"
    }


  extension [T: Numeric](v: Vec2D[T])
    def +(w: Vec2D[T]): Vec2D[T] = (v._1 + w._1, v._2 + w._2)
    def -(w: Vec2D[T]): Vec2D[T] = (v._1 - w._1, v._2 - w._2)
    def *(x: T): Vec2D[T] = (v._1 * x, v._2 * x)

    def rot(deg: Int): Vec2D[T] = (deg % 360 + 360) % 360 match {
      case 90 => (-v._2, v._1)
      case 180 => (-v._1, -v._2)
      case 270 => (v._2, -v._1)
      case 0 => v
    }

    def left(deg: Int): Vec2D[T] = repeat[Vec2D[T]](deg / 90)(_.rotLeft)(v)
    def right(deg: Int): Vec2D[T] = repeat[Vec2D[T]](deg / 90)(_.rotRight)(v)

    def rotLeft: Vec2D[T] = (-v._2, v._1)
    def rotRight: Vec2D[T] = (v._2, -v._1)

    def move(dir: Dir, dist: T): Vec2D[T] = dir match {
      case North => (v._1, v._2 + dist)
      case South => (v._1, v._2 - dist)
      case East => (v._1 + dist, v._2)
      case West => (v._1 - dist, v._2)
    }
    def manhattan: T = v._1.abs + v._2.abs

  def Vec2D[T](x: T, y: T): Vec2D[T] = (x,y)


}
