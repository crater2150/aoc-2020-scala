package aoc2020
import java.lang.Long.parseLong
import cats._
import cats.implicits.given

object day14 extends (List[String] => String) {

  import Mask.Mask
  def apply(input: List[String]): String =
    val s"mask = $firstMask" = input.head
    val maskedWrites = run(input)(s => MaskedMem(Mask(s)))
    val maskedAddrs = run(input)(s => MaskedAddr(Mask.floating(s)))

    val sumOfMemoryMW = maskedWrites.mem.values.sum
    val sumOfMemoryMA = maskedAddrs.mem.values.sum
    s"""Sum of memory (masked writes):    $sumOfMemoryMW
       |Sum of memory (masked addresses): $sumOfMemoryMA
       """.stripMargin

  def run(input: List[String])(memInit: String => Mem): Mem =
    val s"mask = $firstMask" = input.head
    input.tail.foldLeft(memInit(firstMask)){
      case (mem, s"mask = $mask") => mem.updateMask(mask)
      case (mem, s"mem[$addr] = $value") => mem.update(addr.toInt, value.toLong)
    }


  trait Mem {
    def mem: Map[Long, Long]
    def update(addr: Long, value: Long): Mem
    def updateMask(maskCode: String): Mem
  }

  case class MaskedMem(mask: Mask, mem: Map[Long, Long] = Map()) extends Mem {
    def update(addr: Long, value: Long): MaskedMem =
      copy(mem = mem.updated(addr, mask(value)))

    def updateMask(maskCode: String): MaskedMem =
      copy(mask = Mask(maskCode))
  }

  case class MaskedAddr(masks: List[Mask], mem: Map[Long, Long] = Map()) extends Mem {
    def update(addr: Long, value: Long): MaskedAddr =
      copy(mem = mem ++ masks.map(m => m(addr) -> value))

    def updateMask(maskCode: String): MaskedAddr =
      copy(masks = Mask.floating(maskCode))
  }

  object Mask {
    opaque type Mask = (Long, Long)
    def apply(str: String): Mask =
      (parseLong(str.replace('X','0'), 2), parseLong(str.replace('X', '1'), 2))

    def floating(str: String): List[Mask] =
      str.map{
        case 'X' => List('0', '1')
        case '0' => List('X')
        case '1' => List('1')
      }.toList
        .sequence[List, Char]
        .map(s => Mask(s.mkString))

    extension (m: Mask)
      def apply(value: Long): Long = (value | m._1) & m._2
  }
}
