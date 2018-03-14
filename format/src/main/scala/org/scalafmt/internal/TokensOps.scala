package org.scalafmt.internal

import org.scalafmt.internal.Order.{LT, GT, EQ}

import scala.meta.{Token, Tokens}
import scala.meta.Token._

import scala.collection.SeqView
import scala.collection.immutable.IndexedSeq

object TokensOps {
  import TokenOps._

  private def partialOrder(a: Token, b: Token): Int = {
    (a, b) match {
      case (_: Interpolation.Part, _: Interpolation.SpliceStart) => LT
      case (_: Interpolation.SpliceStart, _: Interpolation.Part) => GT
      case (_: Interpolation.SpliceEnd, _: Interpolation.Part) => LT
      case (_: Interpolation.Part, _: Interpolation.SpliceEnd) => GT
      case (_: BOF, _) => LT
      case (_, _: BOF) => GT
      case (_: EOF, _) => GT
      case (_, _: EOF) => LT
      case _ =>
        sys.error(s"undefined token partial order: ${a.show} ??? ${b.show}")
    }
  }

  private def cmp(x: Int, y: Int): Int =
    if (x < y) LT
    else if (x > y) GT
    else EQ

  private implicit val tokensOrder: Order[Token] =
    new Order[Token] {
      def compare(a: Token, b: Token): Int = {
        if (a == b) EQ
        else {
          val cmpEnd = cmp(a.end, b.end)
          if (cmpEnd == EQ) {
            val cmpStart = cmp(a.start, b.start)
            if (cmpStart != EQ) cmpStart
            else partialOrder(a, b)
          } else cmpEnd
        }
      }
    }

  implicit class XtensionTokens(private val tokens: Tokens) extends AnyVal {
    def binarySearch(token: Token): Option[Int] = {
      val res = Searching.search(tokens, token)
      if (res >= 0) Some(res)
      else None
    }

    def trailings(token: Token): SeqView[Token, IndexedSeq[Token]] =
      tokens.view(get(token) + 1, tokens.length)

    def leadings(token: Token): SeqView[Token, IndexedSeq[Token]] =
      tokens.view(0, get(token)).reverse

    def find(start: Token)(p: Token => Boolean): Option[Token] =
      tokens.drop(get(start)).find(p)

    def slice(from: Token, to: Token): Tokens =
      tokens.slice(get(from), get(to))

    /** Returns the next/trailing token or the original token if none exists.
     *
     * @note You need to guard against infinite recursion if iterating through
     *       a list of tokens using this method. This method does not fail
     *       with an exception.
     */
    def next(token: Token): Token = {
      binarySearch(token) match {
        case Some(i) if tokens.length > i + 1 =>
          tokens(i + 1)
        case _ => token
      }
    }

    /** Returns the previous/leading token or the original token if none exists.
     *
     * @note You need to guard against infinite recursion if iterating through
     *       a list of tokens using this method. This method does not fail
     *       with an exception.
     */
    def prev(token: Token): Token = {
      binarySearch(token) match {
        case Some(i) if i > 0 =>
          tokens(i - 1)
        case _ => token
      }
    }

    def leadingSpaces(token: Token): SeqView[Token, IndexedSeq[Token]] =
      leadings(token).takeWhile(_.is[Token.Space])

    def trailingSpaces(token: Token): SeqView[Token, IndexedSeq[Token]] =
      trailings(token).takeWhile(_.is[Token.Space])

    private def get(token: Token): Int =
      binarySearch(token).getOrElse(
        throw new NoSuchElementException(s"token not found: $token")
      )
  }
}
