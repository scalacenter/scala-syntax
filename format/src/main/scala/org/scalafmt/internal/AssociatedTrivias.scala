package org.scalafmt.internal

import org.scalafmt.internal.TokensOps._

import scala.meta._
import scala.meta.Token
import scala.meta.Token.Comment
import scala.meta.contrib._

import org.scalameta.logger

final case class AssociatedTrivias(
    leadings: Map[Token, Tokens],
    trailings: Map[Token, Tokens]
) {
  private def pretty(token: Token): String = {
    if (token.is[Token.BOF]) {
      "BOF"
    } else {
      token.syntax
    }
  }
  private def pretty(association: Map[Token, Tokens]): String =
    association.toList
      .sortBy {
        case (tok, tokens) =>
          (tok.start, tok.end, tokens.start, tokens.end)
      }
      .map {
        case (tok, tokens) =>
          val tokensStructure = tokens
            .map(token => logger.revealWhitespace(pretty(token)))
            .mkString("[", ",", "]")
          s"    ${tok.structure} => $tokensStructure"
      }
      .mkString("\n")
  def syntax: String =
    s"""|AssociatedTrivias(
        |  Leading =
        |${pretty(leadings)}
        |  Trailing =
        |${pretty(trailings)}
        |)""".stripMargin
  override def toString: String = syntax
}
object AssociatedTrivias {
  def apply(tree: Tree): AssociatedTrivias = apply(tree.tokens)
  def apply(tokens: Tokens): AssociatedTrivias = {
    val allLeadings = Map.newBuilder[Token, Tokens]
    val allTrailings = Map.newBuilder[Token, Tokens]

    var leadingStart: Option[Token] = None
    var lastToken: Option[Token] = None
    var isLeading = true

    def setTrivia(t: Token): Unit = {
      if (isLeading && leadingStart.isEmpty) {
        leadingStart = Some(t)
      }
    }

    def doTrailing(currentToken: Token): Unit = {
      lastToken.foreach { last =>
        val slice = tokens.slice(last, currentToken).drop(1)
        if (slice.nonEmpty) {
          allTrailings += last -> slice
        }
      }
      lastToken = None
    }

    tokens.foreach {
      case t: Comment =>
        setTrivia(t)

      case t: Token.BOF =>
        ()

      case t @ Token.LF() =>
        setTrivia(t)
        doTrailing(t)
        isLeading = true

      case t @ Trivia() =>
        setTrivia(t)

      case currentToken =>
        doTrailing(currentToken)
        leadingStart.foreach { start =>
          val slice = tokens.slice(start, currentToken)
          if (slice.nonEmpty) {
            allLeadings += currentToken -> slice
          }
        }
        leadingStart = None
        lastToken = Some(currentToken)
        isLeading = false
    }
    AssociatedTrivias(allLeadings.result(), allTrailings.result())
  }
}
