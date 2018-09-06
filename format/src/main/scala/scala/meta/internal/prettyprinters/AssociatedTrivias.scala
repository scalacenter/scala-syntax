package scala.meta.internal.prettyprinters

import TokensOps._
import Comments._

import scala.meta._
import scala.meta.Token
import scala.meta.Token._
import scala.meta.contrib._

import scala.meta.internal.paiges.Doc
import scala.meta.internal.paiges.Doc._

import org.scalameta.logger

import scala.collection.mutable

final case class AssociatedTrivias(
    allLeadings: Map[Token, Tokens],
    allTrailings: Map[Token, Tokens]
) {

  val attached = mutable.Set[Token]()

  def leadings(token: Token): Option[Tokens] =
    if (attached.contains(token)) None
    else allLeadings.get(token)

  def trailings(token: Token): Option[Tokens] =
    if (attached.contains(token)) None
    else allTrailings.get(token)

  def hasTrailingComment(tree: Tree): Boolean = {
    if (tree.hasTokens) {
      trailings(tree.tokens.last).map(_.exists(_.is[Comment])).getOrElse(false)
    } else false
  }

  def wrap(tree: Tree, doc: Doc): Doc = {
    if (tree.hasTokens) {
      val tokens = tree.tokens.filterNot(_.is[Trivia])
      if (tokens.nonEmpty) {
        val firstToken = tokens.head
        val lastToken = tokens.last

        val l = leadings(firstToken)
        val t = trailings(lastToken)

        attached += firstToken
        attached += lastToken

        wrap(
          l,
          doc,
          t,
          isSeparator = false
        )
      } else doc
    } else doc
  }

  def wrap(
      tree: Tree,
      token: => Token,
      doc: Doc,
      isSeparator: Boolean = false
  ): Doc = {
    if (tree.hasTokens) {
      wrap(
        leadings(token),
        doc,
        trailings(token),
        isSeparator
      )
    } else doc
  }

  def addLeading(
      tree: Tree,
      token: => Token,
      doc: Doc
  ): Doc = {
    if (tree.hasTokens) {
      addLeading(
        leadings(token),
        doc
      )
    } else doc
  }

  def addTrailing(
      tree: Tree,
      token: => Token,
      doc: Doc,
      isSeparator: Boolean = false
  ): Doc = {
    if (tree.hasTokens) {
      addTrailing(
        trailings(token),
        doc
      )
    } else doc
  }

  private def dropIndentations(ts: Seq[Token]): Seq[Token] = {
    val comment = ts.indexWhere(_.is[Comment])
    val before = ts.slice(0, comment)
    val after = ts.slice(comment, ts.size)
    before.filter(_.is[LF]) ++ after
  }

  private def toDoc(
      tokens: Option[Seq[Token]],
      isLeading: Boolean,
      isSeparator: Boolean = false
  ): Doc = {
    tokens match {
      case Some(ts) => {
        val hasComment = ts.exists(_.is[Comment])
        if (hasComment) {
          val ts2 =
            if (isLeading) dropIndentations(ts)
            else ts

          val result = cat(ts2.map {
            case _: LF =>
              lineNoFlatNoIndent

            case _: Space =>
              space

            case c: Comment => {
              val lines = c.text.split("\n")

              def indentSize(line: String): Int =
                line.view.takeWhile(_ == ' ').size

              val indentSize0 =
                ts.filter(!_.is[LF]).takeWhile(!_.is[Comment]).size

              var first = true
              lines.foldLeft(empty) {
                case (doc, commentLine) =>
                  val unindented = commentLine.dropWhile(_ == ' ')

                  val align =
                    if (first) {
                      first = false
                      empty
                    } else {
                      val indentDiff = indentSize(commentLine) - indentSize0
                      lineNoFlat + spaces(indentDiff)
                    }

                  doc + align + text(unindented)
              }
            }
            case e =>
              text(e.text)
          })

          if (!isSeparator) result
          else {
            val endsWithSpace = ts2.lastOption.map(_.is[Space]).getOrElse(false)
            if (endsWithSpace) result
            else result + space
          }
        } else {
          if (isSeparator) space
          else empty
        }
      }
      case None => {
        if (isSeparator) space
        else empty
      }
    }
  }

  private def wrap(
      leadings: Option[Seq[Token]],
      doc: Doc,
      trailings: Option[Seq[Token]],
      isSeparator: Boolean
  ): Doc = {

    val leading =
      toDoc(
        leadings,
        isLeading = true
      )

    val trailing =
      toDoc(
        trailings,
        isLeading = false,
        isSeparator = isSeparator
      )
    leading + doc + trailing
  }

  private def addLeading(leadings: Option[Seq[Token]], doc: Doc): Doc =
    toDoc(leadings, isLeading = true) + doc

  private def addTrailing(trailing: Option[Seq[Token]], doc: Doc): Doc =
    doc + toDoc(trailing, isLeading = false)

  def syntax: String = {
    def prettyToken(token: Token): String = {
      if (token.is[Token.BOF]) {
        "BOF"
      } else {
        token.syntax
      }
    }
    def pretty(association: Map[Token, Tokens]): String =
      association.toList
        .sortBy {
          case (tok, tokens) =>
            (tok.start, tok.end, tokens.start, tokens.end)
        }
        .map {
          case (tok, tokens) =>
            val tokensStructure = tokens
              .map(token => logger.revealWhitespace(prettyToken(token)))
              .mkString("[", ",", "]")
            s"    ${tok.structure} => $tokensStructure"
        }
        .mkString("\n")

    s"""|AssociatedTrivias(
        |  Leading =
        |${pretty(allLeadings)}
        |  Trailing =
        |${pretty(allTrailings)}
        |)""".stripMargin
  }
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
        val start = last
        val end = currentToken
        val includingEnd = end.is[LF]

        val slice = tokens.slice2(
          from = start,
          to = end,
          includeFrom = false,
          includeTo = includingEnd
        )

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

      case t: Token.EOF =>
        doTrailing(t)

      case t @ Token.LF() =>
        setTrivia(t)
        doTrailing(t)
        isLeading = true

      case t @ Trivia() =>
        setTrivia(t)

      case currentToken =>
        doTrailing(currentToken)
        leadingStart.foreach { start =>
          val end = currentToken
          val slice = tokens.slice2(
            from = start,
            to = end
          )

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
