package org.scalafmt.internal

import scala.meta.{Token, Tokens}
import scala.collection.SeqView
import scala.collection.immutable.IndexedSeq
import Order.{LT, GT, EQ}

object TokensOps {

  private implicit val tokensOrder: Order[Token] = 
    new Order[Token] {
      def compare(x: Token, y: Token): Int = {
        if(x == y) EQ
        else {
          if(x.end < y.end) LT
          else {
            if (x.start < y.start) LT
            else GT
          }
        }
      }
    }

  implicit class XtensionTokens(private val tokens: Tokens) extends AnyVal {
    def binarySearch(token: Token): Option[Int] = {
      val res = Searching.search(tokens, token)
      if(res >= 0) Some(res)
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
        throw new NoSuchElementException(s"token not found: ${token.structure}")
      )
  }
}
