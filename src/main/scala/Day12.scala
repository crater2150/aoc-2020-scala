package aoc2020
import aoc2020.lib._
import aoc2020.lib.Vectors._

def day12(input: List[String]): String =
  val directions = Instruction.parse(input)
  val path1 = Ship(East, Vec2D(0,0)).path(directions)
  val path2 = WaypointShip(Vec2D(0,0), Vec2D(10,1)).path(directions)
 
  // Output
  val path1Tikz = tikzPath(path1.map(_.pos), opts = "thick")
  val path2Tikz = tikzPath(path2.map(_.ship), scale = 0.001, opts = "thick")
  val finalPos1 = path1.last
  val finalPos2 = path2.last

  s""" Final position (direct orders): ${finalPos1.pos} (L₁: ${finalPos1.pos.manhattan}), facing ${finalPos1.look.str}
     | Final position (waypoint):      ${finalPos2.ship} (L₁: ${finalPos2.ship.manhattan}), waypoint at ${finalPos2.nav}
     |
     | Tikz path for direct orders:
     | $path1Tikz
     |
     | Tikz path for waypoint navigation:
     | $path2Tikz
     |""".stripMargin


enum Instruction {
  case Rot(deg: Dir)
  case Move(dir: Dir, dist: Int)
  case Forward(dist: Int)
}
import Instruction._
object Instruction {
  def parse(input: List[String]): List[Instruction] = 
  input.map(s => (s(0), s.substring(1).nn.toInt) match {
    case ('L', deg) => Rot(Dir(deg))
    case ('R', deg) => Rot(Dir(-deg))
    case ('F', dist) => Forward(dist)
    case (dir, dist) => Move(dir.cardinal, dist)
  })
}

trait Nav[T] { self: T =>
  def follow(directions: List[Instruction]): T =
    directions.foldLeft(self)(_ follow _)

  def path(directions: List[Instruction]): List[T] =
    directions.scanLeft(self)(_ follow _)

  def follow(command: Instruction): T & Nav[T]
}

/* Part 1: Navigate by interpreting instructions as relative ship movements */
case class Ship(look: Dir, pos: Vec2D[Int]) extends Nav[Ship] {
  def rotate(deg: Dir) = copy(look = look + deg)
  def move(dir: Dir, dist: Int) = copy(pos = pos.move(dir, dist))

  def follow(command: Instruction): Ship = command match {
    case Rot(deg)  => rotate(deg)
    case Forward(dist) => move(look, dist)
    case Move(dir, dist) => move(dir, dist)
  }
}

/* Part 2: Navigate by interpreting instructions as moving a waypoint relative
 * to the ship */
case class WaypointShip(ship: Vec2D[Int], nav: Vec2D[Int]) extends Nav[WaypointShip] {
  def rotate(deg: Dir) = copy(nav = nav.rot(deg))
  def move(dir: Dir, dist: Int) = copy(nav = nav.move(dir, dist))
  def forward(factor: Int) = copy(ship = ship + nav * factor)

  def follow(command: Instruction): WaypointShip = command match {
    case Rot(deg)  => rotate(deg)
    case Forward(dist) => forward(dist)
    case Move(dir, dist) => move(dir, dist)
  }
}

def tikzPath(points: List[Vec2D[Int]], scale: Double = 0.1, opts: String = ""): String =
  points.map(p => f"(${p.x * scale}%.4f, ${p.y * scale}%.4f)").mkString(s"\\draw[$opts] ","--", ";")

