package scala.meta.internal.prettyprinters

import Order.{LT, GT, EQ}

import scala.meta.{Token, Tokens}
import scala.meta.Token._

import scala.collection.SeqView
import scala.collection.immutable.IndexedSeq

case object XmlSpliceEndNotFound extends Exception("cannot find xml splice end")

object TokensOps {
  import TokenOps._

  private def partialOrder(a: Token, b: Token): Int = {
    (a, b) match {
      case (_: Interpolation.Part, _: Interpolation.SpliceStart) => LT
      case (_: Interpolation.SpliceStart, _: Interpolation.Part) => GT
      case (_: Interpolation.SpliceEnd, _: Interpolation.Part) => LT
      case (_: Interpolation.Part, _: Interpolation.SpliceEnd) => GT
      case (_: Xml.Part, _: Xml.SpliceStart) => LT
      case (_: Xml.SpliceStart, _: Xml.Part) => GT
      case (_: Xml.SpliceEnd, _: Xml.Part) => LT
      case (_: Xml.Part, _: Xml.SpliceEnd) => GT
      case (_: Xml.SpliceStart, _: Xml.SpliceEnd) => LT
      case (_: BOF, _) => LT
      case (_, _: BOF) => GT
      case (_: EOF, _) => GT
      case (_, _: EOF) => LT
      case _ =>
        sys.error(
          s"""|undefined token partial order: ${a.syntax} ??? ${b.syntax}
           |${a.getClass} ${a.structure}
           |${b.getClass} ${b.structure}"""
        )
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
      slice2(from, to)

    def slice2(
        from: Token,
        to: Token,
        includeFrom: Boolean = true,
        includeTo: Boolean = false
    ): Tokens = {
      val start =
        get(from) + (
          if (includeFrom) 0
          else 1
        )

      val end =
        get(to) + (
          if (includeTo) 1
          else 0
        )

      tokens.slice(start, end)
    }

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
        if (token.is[Token.Xml.SpliceEnd]) {
          throw XmlSpliceEndNotFound
        } else {
          throw new NoSuchElementException(
            s"""|token not found:
                |  ${token}
                |  ${token.structure}
                |  ${token.getClass}""".stripMargin
          )
        }
      )
  }
}
