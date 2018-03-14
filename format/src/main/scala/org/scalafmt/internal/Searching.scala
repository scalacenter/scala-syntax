package org.scalafmt.internal

import scala.{specialized => sp}

object Order {
  final val LT = -1
  final val EQ = 0
  final val GT = 1
}

trait Order[@sp A] {
  def compare(x: A, y: A): Int
}

// Code taken from: https://github.com/non/spire/blob/master/core/shared/src/main/scala/spire/math/Searching.scala
object Searching {

  /** Performs a binary search, returns an index using the same convention as java.util.Arrays.binarySearch. */
  final def search[@sp A](as: IndexedSeq[A], item: A)(
      implicit ev: Order[A]
  ): Int =
    search(as, item, 0, as.length - 1)

  /** Performs a binary search, returns an index using the same convention as java.util.Arrays.binarySearch. */
  final def search[@sp A](as: IndexedSeq[A], item: A, lower: Int, upper: Int)(
      implicit ev: Order[A]
  ): Int = {
    var first = lower
    var last = upper
    while (first <= last) {
      val middle = (first + last) >>> 1

      val compare = ev.compare(as(middle), item)
      if (compare < 0) first = middle + 1
      else if (compare > 0) last = middle - 1
      else return middle
    }
    -first - 1
  }
}
