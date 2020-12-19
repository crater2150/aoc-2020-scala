package aoc2020
import aoc2020.lib._
import Vectors._, Vectors.given
import scala.math.{min, max}

object day17 extends (List[Vector[Boolean]] => String) {
  type Coord3 = Vec3D[Int]
  type Coord4 = Vec4D[Int]

  def apply(input: List[Vector[Boolean]]): String =
    val active = activeSpace(input)(Vec3D(_, _, 0))
    val active4d = activeSpace(input)(Vec4D(_, _, 0, 0))

    val states = (1 to 6).scanLeft(active)((act, _) => step(act))
    val states4d = (1 to 6).scanLeft(active4d)((act, _) => step(act))
    states.map(_.size).mkString("Active cells (3D): ", " →  ", "\n")
      + states4d.map(_.size).mkString("Active cells (4D): ", " →  ", "\n")


  def step[T: Vec](active: Set[T]): Set[T] =
    neighbourhood(active).flatMap((coord) => {
      val actNeigh = coord.neighbours.count(active)
      if (active(coord) && actNeigh == 2) || actNeigh == 3 then Set(coord)
      else Set.empty
    })

  def activeSpace[T](input: List[Vector[Boolean]])(key: (Int, Int) => T): Set[T] =
    (for (line, x) <- input.zipWithIndex
         (act, y) <- line.zipWithIndex
         if act
     yield key(x, y)
    ).toSet

  def neighbourhood[T:Vec](active: Set[T]): Set[T] =
    active ++ (for (coord) <- active
                   inactive <- coord.neighbours.filterNot(active)
               yield inactive)

  /** create a string representation similar to the one in the instructions for
   *  debugging */

  def showField(field: Set[Coord3]): String =
    // maxima in each direction for active space only
    val (xmin, xmax, ymin, ymax, zmin, zmax) =
      field.foldLeft((Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue)){
        case ((xmin, xmax, ymin, ymax, zmin, zmax), (x: Int,y: Int,z: Int)) =>
          (xmin min x, xmax max x, ymin min y, ymax max y, zmin min z, zmax max z)
      }

    (for { z <- zmin to zmax } yield (
         for { x <- xmin to xmax } yield (
              for { y <- ymin to ymax }
              yield if field(Vec3D(x,y,z)) then '#' else '.'
            ).mkString
          ).mkString(s"\n z=$z\n","\n","")
        ).mkString("\n")

  def showField4d(field: Set[Coord4]): String =
    // maxima in each direction for active space only
    val (xmin, xmax, ymin, ymax, zmin, zmax, wmin, wmax) =
      field.foldLeft((Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue)){
        case ((xmin, xmax, ymin, ymax, zmin, zmax, wmin, wmax), (x:Int,y:Int,z:Int,w:Int)) =>
          (xmin min x, xmax max x, ymin min y, ymax max y, zmin min z, zmax max z, wmin min w, wmax max w)
      }
    (for { w <- wmin to wmax } yield (
      for { z <- zmin to zmax } yield (
           for { x <- xmin to xmax } yield (
                for { y <- ymin to ymax }
                yield if field(Vec4D(x,y,z,w)) then '#' else '.'
              ).mkString
            ).mkString(s"\n--- z=$z; w=$w\n","\n","")
          ).mkString
        ).mkString("\n")
}
