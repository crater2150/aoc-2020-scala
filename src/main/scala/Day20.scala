package aoc2020
import aoc2020.lib._
import cats._, cats.implicits.given
import scala.collection.immutable.LazyList

object day20 extends (List[String] => String) {
  import Vectors._
  import Directions._

  def apply(input: List[String]): String =
    val tiles = parseTiles(input)
    val uniqueBorders = tiles.flatMap(_.borders.keys)
                             .foldMap(v => Map(v -> 1))
                             .filter(_._2 == 1)
                             .map(_._1).toSet
    val corners = tiles.filter(_.borders.keys.count(uniqueBorders) == 2)
                       .map(c => c.removeBorders(uniqueBorders))
                       .toSet
    val cornerIds = corners.map(_.id)
    val image = buildImage(tiles.toSet.filterNot(t => cornerIds(t.id)), corners)

    val fullTile = Tile(-1, image)
    val flipped = fullTile.flipHor

    val seaMonstersPerVariant =
      (for (variant <- List(fullTile, fullTile.rotLeft, fullTile.rotHalf, fullTile.rotRight,
                            flipped,  flipped.rotLeft,  flipped.rotHalf,  flipped.rotRight))
       yield findSeaMonsters(variant.image).length
      ).filter(_ > 0)

    if (seaMonstersPerVariant.length != 1) println("WARNING: Assumption of exactly one orientation of sea monster wrong")

    val seaMonsterArea = seaMonstersPerVariant.head * monsterPattern.map(_.count(_ == '#')).sum
    val blackPixels = image.map(_.count(identity)).sum
    s"""Product of corner ids: ${cornerIds.product}
       |Sea monster total area: $seaMonsterArea
       |Filled pixels: $blackPixels
       |Roughness: ${blackPixels - seaMonsterArea}
       |""".stripMargin

  def parseTiles(input: List[String]): List[Tile] =
    input.split("").foldLeft(List[Tile]())(
      (tiles, lines) => Tile.parse(lines) :: tiles
    )

  /* finds the right arrangement of tiles and creates the full image from them */
  def buildImage(tiles: Set[Tile], corners: Set[Tile]): Vector[Vector[Boolean]] =
    val sideLength = math.sqrt(tiles.size.toDouble + corners.size).toInt
    // Place first corner south west, and rotate open borders to north east
    val start = {
      val dirs = corners.head.borders.values.toSet
      if (dirs(South) && dirs(West)) corners.head.rotRight.rotRight
      else if (dirs(South)) corners.head.rotLeft
      else if (dirs(West)) corners.head.rotRight
      else corners.head
    }

    val toVisitInitial = Set.tabulate(sideLength * sideLength)(i => Vec2D(i % sideLength, i / sideLength))
    concat(placeTiles(Map(Vec2D(0,0) -> start), tiles ++ corners.tail, toVisitInitial), sideLength)

  /* merges arranged tiles into a single large image with the original borders removed */
  def concat(coords: Map[Vec2D[Int], Tile], sideLength: Int): Vector[Vector[Boolean]] =
    def get(x: Int, y: Int) = coords(Vec2D(x, y))

    (for(y <- 0 until sideLength) yield {
      (0 until sideLength).foldLeft(Vector.fill(8)(Vector[Boolean]()))((cols, x) => {
        val tile = get(x, y).image.tail.init
        (cols zip tile).map(_ ++ _.tail.init)
      })
    }).toVector.reverse.combineAll

  /* find the correct position and orientation of all tiles. probably the most annoying part of this puzzle */
  @annotation.tailrec
  def placeTiles(coords: Map[Vec2D[Int], Tile], remaining: Set[Tile], toVisit: Set[Vec2D[Int]]): Map[Vec2D[Int], Tile] =
    def matchDirs(fixedDir: Dir, fixed: Tile, unfixed: Tile): Tile =
      val targetOpposite = fixed.revBorders(fixedDir)
      val targetDir = fixedDir.flip
      var res = unfixed
      while (res.revBorders.get(targetDir).map(_ != targetOpposite).getOrElse(true)) {
        res = res.rotLeft
      } 
      val bordersMatch = res.revBorders(targetDir) matches targetOpposite
      targetDir match
            case North|South => if !bordersMatch then res.flipHor else res
            case East|West => if !bordersMatch then res.flipVert else res

    /* probably could be sped up a little bit more by removing already connected borders, but it isn't too slow and
     * already ugly enough */
    toVisit.find(coords.keySet) match {
      case None => coords
      case Some(pos) =>
        val current = coords(pos)

        val placements = for (curBorder, dir) <- current.borders
                                    neighbour <- remaining.find(_.borders.contains(curBorder))
                         yield (pos.move(dir, 1) -> matchDirs(dir, current, neighbour), neighbour)

        val (placed, neighbours) = placements.unzip
        val fixed = coords ++ placed

        placeTiles(fixed, remaining -- neighbours, toVisit - pos)
    }

  /* a mighty sea monster */
  val monsterPattern = Vector(
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   ",
    )

  val monsterCoords: IndexedSeq[(Int, Int)] = for {
    y <- 0 until monsterPattern.length
    x <- 0 until monsterPattern(y).length
    if monsterPattern(y)(x) == '#'
  } yield (y,x)


  def findSeaMonsters(image: Vector[Vector[Boolean]]) =
    for {
      y <- 0 until image.length - monsterPattern.length
      x <- 0 until image(y).length - monsterPattern(0).length
      if monsterCoords.forall((yOff,xOff) => image(y + yOff)(x + xOff))
    } yield (y,x)
 
  trait Tile:
    def id: Long
    def image: Vector[Vector[Boolean]]
    def flipVert: Tile = Tile(id, image.reverse)
    def flipHor: Tile = Tile(id, image.map(_.reverse))
    def rotLeft: Tile = Tile(id, image.map(_.reverse).transpose)
    def rotRight: Tile = Tile(id, image.transpose.map(_.reverse))
    def rotHalf: Tile = Tile(id, image.reverse.map(_.reverse))

    def borders: Map[Border, Dir]
    def revBorders = borders.map(_.swap)
    def removeBorders(unique: Set[Border]): Tile
    protected def autoBorders = Map(
      Border(image.head) -> North, 
      Border(image.last) -> South, 
      Border(image.map(_.head)) -> West,
      Border(image.map(_.last)) -> East,
      )

  case class AnyTile(id: Long, image: Vector[Vector[Boolean]]) extends Tile:
    def borders = autoBorders
    def removeBorders(unique: Set[Border]): Tile = FilteredBorderTile(id, image, unique)

  case class FilteredBorderTile(id: Long, image: Vector[Vector[Boolean]], uniqueRemoved: Set[Border]) extends Tile:
    def borders = autoBorders -- uniqueRemoved
    def removeBorders(unique: Set[Border]): Tile = FilteredBorderTile(id, image, uniqueRemoved ++ unique)

  val EmptyTile = Tile(0, Vector.fill(10)(Vector.fill(10)(false)))

  object Tile {
    def apply(id: Long, image: Vector[Vector[Boolean]]): Tile =
      AnyTile(id, image)

    given Show[Tile]:
      def show(t: Tile): String = t.image.map(_.map(if _ then '#' else '.').mkString).mkString("\n")

    def parse(lines: List[String]): Tile =
      val s"Tile $id:" = lines.head
      val rows = lines.tail.map(boolChar('#')).toVector
      Tile(id.toLong, rows)
  }

  /* Border class with custom equals, that also accepts mirrored borders */
  final class Border(val bits: Vector[Boolean]) {
    val reverse = bits.reverse
    def canEqual(a: Any): Boolean = a.isInstanceOf[Border]
    override def equals(x: Any): Boolean = x match {
      case that: Border =>
        that.canEqual(this) && (
          that.bits == this.bits || that.bits == this.reverse
        )
      case _ => false
    } 
    def matches(that: Border) = bits == that.bits

    override def hashCode(): Int = bits.hashCode * reverse.hashCode
    override def toString(): String = bits.map(if _ then '#' else '.').mkString

    def flip: Border = Border(reverse)
  }

}

