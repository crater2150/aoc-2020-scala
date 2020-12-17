package aoc2020
import aoc2020.lib._
import Vectors._, Vectors.given
import scala.math.{min, max}

type Coord3 = Vec3D[Int]
type Coord4 = Vec4D[Int]

def day17(input: List[Vector[Boolean]]): String =
  val active = activeSpace(input)(Vec3D(_, _, 0))
  val active4d = activeSpace(input)(Vec4D(_, _, 0, 0))

  val states = (1 to 6).scanLeft(active)((act, _) => step(act))
  val states4d = (1 to 6).scanLeft(active4d)((act, _) => step(act))
  states.map(_.count(_._2)).mkString("Active cells (3D): ", " →  ", "\n")
    + states4d.map(_.count(_._2)).mkString("Active cells (4D): ", " →  ", "\n")


def step[T: Vec](active: Map[T, Boolean]): Map[T, Boolean] =
  neighbourhood(active).map((coord, act) => {
    val actNeigh = coord.neighbours.count(active.getOrElse(_, false))
    val actNext = (act && actNeigh == 2) || actNeigh == 3
    coord -> actNext
  })

def activeSpace[T](input: List[Vector[Boolean]])(key: (Int, Int) => T): Map[T, Boolean] =
  (for (line, x) <- input.zipWithIndex
       (act, y) <- line.zipWithIndex
   yield key(x, y) -> act
  ) .toMap

def neighbourhood[T:Vec](active: Map[T, Boolean]): Map[T, Boolean] =
  active ++ (for (coord, _) <- active.filter(_._2)
                 inactive <- coord.neighbours.filterNot(active.contains)
             yield inactive -> false
            ).toMap

/** create a string representation similar to the one in the instructions for
 *  debugging */

def showField(field: Map[Coord3, Boolean]): String =
  // maxima in each direction for active space only
  val (xmin, xmax, ymin, ymax, zmin, zmax) =
    field.foldLeft((Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue)){
      case ((xmin, xmax, ymin, ymax, zmin, zmax), (x: Int,y: Int,z: Int) -> true) =>
        (xmin min x, xmax max x, ymin min y, ymax max y, zmin min z, zmax max z)
      case (extrema, _ -> false) => extrema
    }
  (for { z <- zmin to zmax } yield (
       for { x <- xmin to xmax } yield (
            for { y <- ymin to ymax }
            yield if field.getOrElse(Vec3D(x,y,z), false) then '#' else '.'
          ).mkString
        ).mkString(s"\n z=$z\n","\n","")
      ).mkString("\n")

def showField4d(field: Map[Coord4, Boolean]): String =
  // maxima in each direction for active space only
  val (xmin, xmax, ymin, ymax, zmin, zmax, wmin, wmax) =
    field.foldLeft((Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue)){
      case ((xmin, xmax, ymin, ymax, zmin, zmax, wmin, wmax), (x:Int,y:Int,z:Int,w:Int) -> true) =>
        (xmin min x, xmax max x, ymin min y, ymax max y, zmin min z, zmax max z, wmin min w, wmax max w)
      case (extrema, _ -> false) => extrema
    }
  (for { w <- wmin to wmax } yield (
    for { z <- zmin to zmax } yield (
         for { x <- xmin to xmax } yield (
              for { y <- ymin to ymax }
              yield if field.getOrElse(Vec4D(x,y,z,w), false) then '#' else '.'
            ).mkString
          ).mkString(s"\n--- z=$z; w=$w\n","\n","")
        ).mkString
      ).mkString("\n")
