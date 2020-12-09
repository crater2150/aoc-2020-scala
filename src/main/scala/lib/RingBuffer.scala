package aoc2020.lib

import scala.collection.immutable._
import scala.annotation.alpha

/* Wrap a vector to create a limited size ring buffer */

case class RingBuffer[A](underlying: Vector[A], maxSize: Int) {
  /* pass through any methods to the underlying vector, except appending and
   * prepending single elements */
  export underlying.{ :+ => _, appended => _, +: => _, prepended => _, _ }

  inline def full: Boolean = underlying.size == maxSize

  //TODO alpha is renamed to targetName in M2 or M3
  @alpha("appended") def :+[B >: A](b: B): RingBuffer[B] =
    val appendedVec = (if full then underlying.tail else underlying) :+ b
    new RingBuffer(appendedVec, maxSize)

  @alpha("prepended") def +:[B >: A](b: B): RingBuffer[B] =
    val prependedVec = b +: (if full then underlying.init else underlying)
    new RingBuffer(prependedVec, maxSize)

}

object RingBuffer {
  def apply[A](underlying: Vector[A], maxSize: Int): RingBuffer[A] =
    new RingBuffer(
      if underlying.size > maxSize then underlying.view.slice(0, maxSize).toVector 
      else underlying,
      maxSize
    )
}
