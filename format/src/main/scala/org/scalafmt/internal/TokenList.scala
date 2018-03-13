package org.scalafmt.internal

import scala.collection.SeqView
import scala.collection.immutable.IndexedSeq
import scala.meta.tokens.Token
import scala.meta.tokens.Tokens

/** Helper to traverse tokens as a doubly linked list.  */
final class TokenList private (tokens: Tokens) {
  import TokenOps._

  def trailings(token: Token): SeqView[Token, IndexedSeq[Token]] =
    tokens.view(get(token), tokens.length)

  def leadings(token: Token): SeqView[Token, IndexedSeq[Token]] =
    tokens.view(0, get(token)).reverse

  def get(token: Token): Int =
    tokens
      .binarySearch(token)
      .getOrElse(
        throw new NoSuchElementException(s"token not found: $token")
      )

  def find(start: Token)(p: Token => Boolean): Option[Token] =
    tokens.drop(get(start)).find(p)

  def slice(from: Token, to: Token): Seq[Token] =
    tokens.view(get(from), get(to))

  def leadingSpaces(token: Token): SeqView[Token, IndexedSeq[Token]] =
    leadings(token).takeWhile(_.is[Token.Space])

  def trailingSpaces(token: Token): SeqView[Token, IndexedSeq[Token]] =
    trailings(token).takeWhile(_.is[Token.Space])
}

object TokenList {
  def apply(tokens: Tokens): TokenList = new TokenList(tokens)
}
