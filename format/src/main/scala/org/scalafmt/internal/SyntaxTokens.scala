package org.scalafmt.internal

import scala.meta._
import scala.meta.Token
import scala.meta.tokens.Token._
import scala.meta.classifiers.Classifier

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
 * see https://blog.codinghorror.com/ascii-pronunciation-rules-for-programmers/
 */
object SyntaxTokens {
  implicit class XtensionTokens(val tokens: Tokens) extends AnyVal {
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
      val res = loop(0, tokens.length - 1)
      if (res == -1) None
      else Some(res)
    }
  }
  private implicit class XtensionUtil[A <: Tree](val tree: A) extends AnyVal {
    def find[T <: Token](implicit ev: Classifier[Token, T]): Option[T] = {
      find[T](tree.tokens)(ev)
    }
    def findAfter[T <: Token](
        p: A => Tree
    )(implicit ev: Classifier[Token, T]): Option[T] = {
      after(p, tree.tokens).flatMap(tokens => find[T](tokens)(ev))
    }
    def findAfter(f: Token => Boolean, p: A => Tree): Option[Token] = {
      after(p, tree.tokens).flatMap(tokens => tokens.find(f))
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
      val end = p(tree).tokens.last
      tokens.binarySearch(end).map(tokens.drop).map(_.drop(1))
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
  implicit class XtensionTermApplyTypeSyntax(val tree: Term.ApplyType)
      extends AnyVal {
    def tokensLeftBracket: LeftBracket =
      tree.findBetween[LeftBracket](_.fun, _.targs.head).get
    def tokensRightBracket: RightBracket =
      tree.findAfter[RightBracket](_.targs.last).get
  }
  implicit class XtensionTermAssignSyntax(val tree: Term.Assign)
      extends AnyVal {
    def tokensEqual: Equals = tree.findAfter[Equals](_.lhs).get
  }
  implicit class XtensionTermEtaSyntax(val tree: Term.Eta) extends AnyVal {
    def tokensUnderscore: Underscore = tree.find[Underscore].get
  }
  implicit class XtensionTermThisSyntax(val tree: Term.This) extends AnyVal {
    def tokensDot: Option[Dot] = {
      tree.qual match {
        case _: Name.Anonymous => None
        case _ => tree.findAfter[Dot](_.qual)
      }
    }
  }
  implicit class XtensionTermSuperSyntax(val tree: Term.Super) extends AnyVal {
    def tokensDot: Option[Dot] = {
      tree.thisp match {
        case _: Name.Anonymous => None
        case _ => tree.find[Dot]
      }
    }
  }
  implicit class XtensionTermSelectSyntax(val tree: Term.Select)
      extends AnyVal {
    def tokensDot: Dot = tree.findAfter[Dot](_.qual).get
  }
  implicit class XtensionTermInterpolateSyntax(val tree: Term.Interpolate)
      extends AnyVal {
    def tokensStartQuote: Interpolation.Start =
      tree.find[Interpolation.Start].get
    def tokensEndQuote: Interpolation.End = tree.find[Interpolation.End].get
  }
  implicit class XtensionTermReturnSyntax(val tree: Term.Return)
      extends AnyVal {
    def tokensReturn: KwReturn = tree.find[KwReturn].get
  }
  implicit class XtensionTermRepeatedSyntax(val tree: Term.Repeated)
      extends AnyVal {
    def tokensColon: Colon = tree.findAfter[Colon](_.expr).get
    def tokensUnderscore: Underscore = tree.findAfter[Underscore](_.expr).get
    def tokensAsterix: Token = tree.findAfter(isAsterisk, _.expr).get
  }
  implicit class XtensionTermIfSyntax(val tree: Term.If) extends AnyVal {
    def tokensIf: KwIf = tree.find[KwIf].get
    def tokensLeftParen: LeftParen = tree.find[LeftParen].get
    def tokensRightParen: RightParen = tree.findAfter[RightParen](_.cond).get
    def tokensElse: Option[KwElse] = tree.findAfter[KwElse](_.thenp)
  }
  implicit class XtensionTermForSyntax(val tree: Term.For) extends AnyVal {
    def tokensFor: KwFor = tree.find[KwFor].get
  }
  implicit class XtensionTermForYieldSyntax(val tree: Term.ForYield)
      extends AnyVal {
    def tokensFor: KwFor = tree.find[KwFor].get
    def tokensYield: KwYield = tree.findAfter[KwYield](_.enums.last).get
  }
  implicit class XtensionTermBlockSyntax(val tree: Term.Block) extends AnyVal {
    def tokensLeftBrace: LeftBrace = blockStartBrace(tree)
    def tokensRightBrace: RightBrace = blockEndBrace(tree)(_.stats)
  }
  implicit class XtensionTermDoSyntax(val tree: Term.Do) extends AnyVal {
    def tokensDo: KwDo = tree.find[KwDo].get
    def tokensWhile: KwWhile = tree.findBetween[KwWhile](_.body, _.expr).get
    def tokensLeftParen: LeftParen =
      tree.findBetween[LeftParen](_.body, _.expr).get
    def tokensRightParen: RightParen = tree.findAfter[RightParen](_.expr).get
  }
  implicit class XtensionTermPartialFunctionSyntax(
      val tree: Term.PartialFunction
  ) extends AnyVal {
    def tokensLeftBrace: LeftBrace = blockStartBrace(tree)
    def tokensRightBrace: RightBrace = blockEndBrace(tree)(_.cases)
  }
  implicit class XtensionTermParamSyntax(val tree: Term.Param) extends AnyVal {
    def tokenColon: Option[Colon] =
      tree.decltpe.map(
        decltpe => tree.findBetween[Colon](_.name, _ => decltpe).get
      )
    def tokenEqual: Option[Equals] =
      tree.default.map(
        default =>
          tree.decltpe match {
            case Some(decltpe) =>
              tree.findBetween[Equals](_ => decltpe, _ => default).get
            case None => tree.findAfter[Equals](_.name).get
        }
      )
  }
  implicit class XtensionTermFunctionSyntax(val tree: Term.Function)
      extends AnyVal {
    def tokensRightArrow: RightArrow =
      tree.findAfter[RightArrow](_.params.last).get
  }
  implicit class XtensionTermThrowSyntax(val tree: Term.Throw) extends AnyVal {
    def tokensThrow: KwThrow = tree.find[KwThrow].get
  }
  implicit class XtensionTermAscribeSyntax(val tree: Term.Ascribe)
      extends AnyVal {
    def tokensColon: Colon = tree.findAfter[Colon](_.expr).get
  }
  implicit class XtensionTermAnnotateSyntax(val tree: Term.Annotate)
      extends AnyVal {
    def tokensAt: At = tree.findAfter[At](_.expr).get
  }
  implicit class XtensionTermTupleSyntax(val tree: Term.Tuple) extends AnyVal {
    def tokensLeftParen: LeftParen = tree.find[LeftParen].get
    def tokensRightParen: RightParen =
      tree.findAfter[RightParen](_.args.last).get
  }
  implicit class XtensionTermMatchSyntax(val tree: Term.Match) extends AnyVal {
    def tokensMatch: KwMatch = tree.findAfter[KwMatch](_.expr).get
  }
  implicit class XtensionTermNewSyntax(val tree: Term.New) extends AnyVal {
    def tokensNew: KwNew = tree.find[KwNew].get
  }
  implicit class XtensionTermNewAnonymousSyntax(val tree: Term.NewAnonymous)
      extends AnyVal {
    def tokensNew: KwNew = tree.find[KwNew].get
  }
  implicit class XtensionTermPlaceholderSyntax(val tree: Term.Placeholder)
      extends AnyVal {
    def tokensUnderscore: Underscore = tree.find[Underscore].get
  }
  implicit class XtensionTermTrySyntax(val tree: Term.Try) extends AnyVal {
    def tokensCatch: Option[KwCatch] =
      tree.catchp.headOption
        .map(catchp => tree.findBetween[KwCatch](_.expr, _ => catchp).get)
    def tokensFinally: Option[KwFinally] =
      tree.finallyp.map { finallyp =>
        val left = tree.catchp.lastOption.getOrElse(tree.expr)
        tree.findBetween[KwFinally](_ => left, _ => finallyp).get
      }
    def tokensLeftParen: Option[LeftParen] = tree.findBefore[LeftParen](_.expr)
    def tokensRightParen: Option[RightParen] = tree.findBetween[RightParen](
      afterP = _.expr,
      beforeP = t =>
        // scalameta#1423 currently, it's possible that both catchp and finallyp are empty
        t.catchp.headOption.getOrElse(
          t.finallyp.getOrElse(
            sys.error(
              "try catchp and finallyp are empty, this should not be possible"
            )
          )
      )
    )
    def tokensTry: KwTry = tree.find[KwTry].get
  }
  implicit class XtensionTermTryWithHandlerSyntax(val tree: Term.TryWithHandler)
      extends AnyVal {
    def tokensCatch: KwCatch = tree.findBetween[KwCatch](_.expr, _.catchp).get
    def tokensFinally: Option[KwFinally] =
      tree.finallyp.map(
        finallyp =>
          tree
            .findBetween[KwFinally](
              _.catchp,
              _ => finallyp
            )
            .get
      )
    def tokensLeftParen: Option[LeftParen] = tree.findBefore[LeftParen](_.expr)
    def tokensRightParen: Option[RightParen] =
      tree.findBetween[RightParen](
        _.expr,
        _.catchp
      )
    def tokensTry: KwTry = tree.find[KwTry].get
  }
  implicit class XtensionTermWhileSyntax(val tree: Term.While) extends AnyVal {
    def tokensWhile: KwWhile = tree.find[KwWhile].get
    def tokensLeftParen: LeftParen = tree.find[LeftParen].get
    def tokensRightParen: RightParen = tree.findAfter[RightParen](_.expr).get
  }
  private def blockStartBrace(tree: Tree): LeftBrace = tree.find[LeftBrace].get
  private def blockEndBrace[T <: Tree](
      tree: T
  )(childs: T => List[Tree]): RightBrace = {
    val stats = childs(tree)
    if (stats.isEmpty) tree.find[RightBrace].get
    else tree.findAfter[RightBrace](_ => stats.last).get
  }
  private def isAsterisk(token: Token): Boolean = token match {
    case Ident(value) => value == "*"
    case _ => false
  }
}
