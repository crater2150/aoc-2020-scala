package aoc2020

object day25 extends (List[Long] => String) {
  def apply(input: List[Long]): String =
    val loopSizes = input.map(findLoopSize(7, _))
    val encryptionKey = transform(input(0), loopSizes(1))
    s"Encryption Key: $encryptionKey"

  val module = 20201227L
  def loopStep(current: Long, subject: Long): Long = current * subject % module

  @annotation.tailrec
  def transform(subject: Long, loopSize: Long, current: Long = 1): Long =
    if loopSize == 0 then current
    else transform(subject, loopSize - 1, loopStep(current, subject))

  def findLoopSize(subject: Long, pubKey: Long): Long =
    def rec(current: Long, iterations: Long = 0): Long =
      if current == pubKey then iterations
      else rec(loopStep(current, subject), iterations + 1)
    rec(1)
}
