package aoc2020

import Coords._


def day11(seatMap: Vector[Vector[Boolean]]): String =
  val height = seatMap.size
  val width = seatMap(0).size

  val coords: Set[Coord] = for {
    y <- (0 until height).toSet
    x <- 0 until width
    if seatMap(y)(x)
  } yield Coord(y, x)
  
  val visibilityLOS: Coord => List[Coord] = _.lineOfSight(height, width)(coords)
  "Basic rules (neighbours < 4)\n" +
    gameOfSeats(occupiedAfterRound(3)(_.neighbours))(coords, Set.empty).toString +
    "\nLOS rules (neighbours < 5)\n" + 
    gameOfSeats(occupiedAfterRound(4)(visibilityLOS))(coords, Set.empty).toString
  


@annotation.tailrec
def gameOfSeats(occupationRule: Set[Coord] => Coord => Boolean)(coords: Set[Coord], states: Set[Coord]): Int =
  val newStates = coords.filter(occupationRule(states))
  if newStates == states then newStates.size
  else gameOfSeats(occupationRule)(coords, newStates)
  


def occupiedAfterRound(maxNeighbours: Int)(visible: Coord => List[Coord])(occupied: Set[Coord])(c: Coord) =
  val occNeighbours = visible(c).count(occupied)
  occNeighbours == 0 || (occupied(c) && occNeighbours <= maxNeighbours)


object Coords {
  opaque type Coord = (Int, Int)

  extension (c: Coord)
    def neighbours: List[Coord] = List(
      (c._1 - 1, c._2),
      (c._1 + 1, c._2),
      (c._1    , c._2 - 1),
      (c._1 - 1, c._2 - 1),
      (c._1 + 1, c._2 - 1),
      (c._1    , c._2 + 1),
      (c._1 - 1, c._2 + 1),
      (c._1 + 1, c._2 + 1),
    )

    def lineOfSight(height: Int, width: Int)(seats: Set[Coord]): List[Coord] =
      def line(dirY: Int, dirX: Int, pos: Coord): Coord =
        val npos = (pos._1 + dirY, pos._2 + dirX)
        if (npos._1 >= height || npos._1 < 0 || npos._2 >= width || npos._2 < 0 || seats(npos))
          npos
        else
          line(dirY, dirX, npos)
      List(
        line(-1,  0, c),
        line(+1,  0, c),
        line( 0, -1, c),
        line( 0, +1, c),
        line(+1, +1, c),
        line(+1, -1, c),
        line(-1, +1, c),
        line(-1, -1, c),
      )


  def Coord(x: Int, y: Int): Coord = (x,y)

  def stateToString(height: Int, width: Int)(seats: Set[Coord], states: Set[Coord]):String =
    Vector.tabulate(height, width)((y,x) =>
        if (!seats((y,x))) '.'
        else if (states((y,x))) '#'
        else 'L'
        )
    .map(_.mkString)
    .mkString("\n")
}



