package org.scalafmt.internal

import scala.meta.{Token, Tokens}
import scala.meta.Token._
import org.scalafmt.internal.TokenOps._

sealed abstract class MatchingParens(map: Map[TokenHash, Token]) {
  private def lookup(token: Token) = map.get(hash(token))
  def close(open: LeftParen): Option[RightParen] =
    lookup(open).collect { case x: RightParen => x }
  def close(open: LeftBracket): Option[RightBracket] =
    lookup(open).collect { case x: RightBracket => x }
  def close(open: LeftBrace): Option[RightBrace] =
    lookup(open).collect { case x: RightBrace => x }
  def open(close: RightParen): Option[LeftParen] =
    lookup(close).collect { case x: LeftParen => x }
  def open(close: RightBracket): Option[LeftBracket] =
    lookup(close).collect { case x: LeftBracket => x }
  def open(close: RightBrace): Option[LeftBrace] =
    lookup(close).collect { case x: LeftBrace => x }
}

object MatchingParens {
  private def assertValidParens(open: Token, close: Token): Unit = {
    (open, close) match {
      case (Interpolation.Start(), Interpolation.End()) =>
      case (LeftBrace(), RightBrace()) =>
      case (LeftBracket(), RightBracket()) =>
      case (LeftParen(), RightParen()) =>
      case (o, c) =>
        throw new IllegalArgumentException(s"Mismatching parens ($o, $c)")
    }
  }

  /**
   * Finds matching parens [({})].
   *
   * Contains lookup keys in both directions, opening [({ and closing })].
   */
  private def getMatchingParentheses(tokens: Tokens): Map[TokenHash, Token] = {
    val ret = Map.newBuilder[TokenHash, Token]
    var stack = List.empty[Token]
    tokens.foreach {
      case open @ (LeftBrace() | LeftBracket() | LeftParen() |
          Interpolation.Start()) =>
        stack = open :: stack
      case close @ (RightBrace() | RightBracket() | RightParen() |
          Interpolation.End()) =>
        val open = stack.head
        assertValidParens(open, close)
        ret += hash(open) -> close
        ret += hash(close) -> open
        stack = stack.tail
      case _ =>
    }
    val result = ret.result()
    result
  }
  def apply(tokens: Tokens): MatchingParens =
    new MatchingParens(getMatchingParentheses(tokens)) {}
}
