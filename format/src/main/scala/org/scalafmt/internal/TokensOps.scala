package org.scalafmt.internal

import scala.meta.{Token, Tokens}
import scala.collection.SeqView
import scala.collection.immutable.IndexedSeq

object TokensOps {
  implicit class XtensionTokens(private val tokens: Tokens) extends AnyVal {
    def binarySearch(token: Token): Option[Int] = {
      def loop(lo: Int, hi: Int): Int = {
        if (lo > hi) -1
        else {
          val mid = lo + ((hi - lo) / 2)
          val guess = tokens(mid)
          if (guess == token) mid
          else if (guess.end < token.end) loop(mid + 1, hi)
          else loop(lo, mid - 1)
        }
      }
      val res = loop(0, tokens.length)
      if (res == -1) None
      else Some(res)
    }

    def trailings(token: Token): SeqView[Token, IndexedSeq[Token]] =
      tokens.view(get(token) + 1, tokens.length)

    def leadings(token: Token): SeqView[Token, IndexedSeq[Token]] =
      tokens.view(0, get(token)).reverse

    def find(start: Token)(p: Token => Boolean): Option[Token] =
      tokens.drop(get(start)).find(p)

    def slice(from: Token, to: Token): Seq[Token] =
      tokens.view(get(from), get(to))

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
