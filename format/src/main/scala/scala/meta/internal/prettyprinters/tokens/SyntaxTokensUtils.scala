package scala.meta.internal.prettyprinters
package tokens

import scala.meta._
import scala.meta.Token._
import scala.meta.classifiers.Classifier

import scala.collection.SeqView
import scala.collection.immutable.IndexedSeq

/**
 * Glossary:
 * `(` left paren
 * `)` right paren
 * `,` comma
 * `.` dot
 * `;` semicolon
 * `[` left bracket
 * `]` right bracket
 * `{` left brace
 * `}` right brace
 * `*` asterisk
 * `&` and
 * `@` at
 * see https://blog.codinghorror.com/ascii-pronunciation-rules-for-programmers/
 */
object SyntaxTokensUtils {
  import TokensOps._

  private[tokens] implicit class XtensionUtil[A <: Tree](private val tree: A)
      extends AnyVal {
    def find[T <: Token](implicit ev: Classifier[Token, T]): Option[T] = {
      find[T](tree.tokens)(ev)
    }
    def findAfter[T <: Token](
        p: A => Tree
    )(implicit ev: Classifier[Token, T]): Option[T] = {
      after(p, tree.tokens).flatMap(tokens => find[T](tokens)(ev))
    }

    def findBefore[T <: Token](
        p: A => Tree
    )(implicit ev: Classifier[Token, T]): Option[T] = {
      before(p, tree.tokens).flatMap(tokens => find[T](tokens)(ev))
    }

    def findBetween[T <: Token](
        afterP: A => Tree,
        beforeP: A => Tree
    )(implicit ev: Classifier[Token, T]): Option[T] = {
      between(afterP, beforeP).flatMap(tokens => find[T](tokens)(ev))
    }
    private def after(p: A => Tree, tokens: Tokens): Option[Tokens] = {
      p(tree).tokens.lastOption.flatMap(end =>
        tokens.binarySearch(end).map(tokens.drop).map(_.drop(1))
      )
    }
    private def before(p: A => Tree, tokens: Tokens): Option[Tokens] = {
      val end = p(tree).tokens.head
      tokens.binarySearch(end).map(tokens.take)
    }
    private def between(
        afterP: A => Tree,
        beforeP: A => Tree
    ): Option[Tokens] = {
      after(afterP, tree.tokens).flatMap(tokens => before(beforeP, tokens))
    }
    private def find[T <: Token](
        tokens: Tokens
    )(implicit ev: Classifier[Token, T]): Option[T] = {
      tokens.find(_.is[T]).map(_.asInstanceOf[T])
    }
  }

  private[tokens] def commaSeparated[T <: Tree](
      tree: T
  )(f: T => List[Tree]): List[Comma] =
    commaSeparated0(tree)(f(tree))

  private[tokens] def commaSeparated0(
      tree: Tree
  )(elems: List[Tree]): List[Comma] = {
    elems match {
      case Nil => Nil
      case _ :: Nil => Nil
      case _ =>
        elems
          .sliding(2, 1)
          .map {
            case List(l, r) => tree.findBetween[Comma](_ => l, _ => r).get
          }
          .toList
    }
  }

  private[tokens] def blockStartBrace(tree: Tree): LeftBrace =
    tree.find[LeftBrace].get
  private[tokens] def blockEndBrace[T <: Tree](
      tree: T
  )(childs: T => List[Tree]): RightBrace = {
    val stats = childs(tree)
    if (stats.isEmpty) tree.find[RightBrace].get
    else tree.findAfter[RightBrace](_ => stats.last).get
  }
}
