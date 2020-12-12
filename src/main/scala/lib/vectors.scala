package aoc2020.lib

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
    def +(deg: Int|Dir): Dir = (dir + deg) % 360
    def -(deg: Int|Dir): Dir = ((dir - deg) % 360 + 360) % 360
    def unary_- : Dir = 360 - dir
    def str: String = dir match {
      case East  => "East"
      case North => "North"
      case West  => "West"
      case South => "South"
    }

  def Dir(deg: Int): Dir = (deg % 360 + 360) % 360


  extension [T: Numeric](v: Vec2D[T])
    def +(w: Vec2D[T]): Vec2D[T] = (v._1 + w._1, v._2 + w._2)
    def -(w: Vec2D[T]): Vec2D[T] = (v._1 - w._1, v._2 - w._2)
    def *(x: T): Vec2D[T] = (v._1 * x, v._2 * x)

    def x: T = v._1
    def y: T = v._2

    def rot(deg: Dir): Vec2D[T] = deg match {
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
