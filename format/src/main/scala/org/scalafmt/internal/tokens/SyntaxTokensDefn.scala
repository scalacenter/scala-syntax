package org.scalafmt.internal
package tokens

import org.scalafmt.internal.{ScalaToken => S}

import scala.meta._
import scala.meta.tokens.Token._

import scala.meta.internal.paiges.Doc

object SyntaxTokensDefn {
  import Defn._
  import TokensOps._
  import SyntaxTokensUtils._

  implicit class XtensionDefnClassSyntax(private val tree: Class)
      extends AnyVal {
    def `class`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensClass, S.`class`)
    }

    def tokensClass: KwClass = tree.findBefore2[KwClass](_.name)

    // tparams
    def tokensLeftBracket: Option[LeftBracket] =
      tree.tparams.headOption
        .map(tparam => tree.findBetween[LeftBracket](_.name, _ => tparam).get)
    def tokensRightBracket: Option[RightBracket] =
      tree.tparams.lastOption.map(
        tparam =>
          tree.ctor.paramss match {
            case Nil => tree.findAfter[RightBracket](_ => tparam).get
            case xs :: _ =>
              xs match {
                case Nil => tree.findAfter[RightBracket](_ => tparam).get
                case x :: _ =>
                  tree.findBetween[RightBracket](_ => tparam, _ => x).get
              }
        }
      )

    def tokensCommaTparams: List[Comma] =
      commaSeparated(tree)(_.tparams)

    def tokensCommaCtor: List[List[Comma]] = {
      import SyntaxTokensMisc.XtensionCtorPrimarySyntax
      tree.ctor.tokensComma
    }
    def tokensParenthesis: List[(LeftParen, RightParen)] = {
      val paramss = tree.ctor.paramss
      if (paramss.isEmpty) Nil
      else {
        val buf = List.newBuilder[(LeftParen, RightParen)]
        val tokens = tree.tokens
        val matching = MatchingParens(tokens)
        val modLast =
          for {
            mod <- tree.ctor.mods.lastOption
            tok <- mod.tokens.lastOption
          } yield tok
        val tparamLast =
          for {
            tparam <- tree.tparams.lastOption
            tok <- tparam.tokens.lastOption
          } yield tok
        val start =
          modLast.orElse(tparamLast).orElse(tree.name.tokens.lastOption).get

        def loop(start: Token, paramss: List[List[Term.Param]]): Unit =
          paramss match {
            case Nil =>
            case _ :: tail =>
              val open = tokens
                .trailings(start)
                .find(_.is[LeftParen])
                .get
                .asInstanceOf[LeftParen]
              val close = matching.close(open).get
              buf += (open -> close)
              loop(close, tail)
          }
        loop(start, tree.ctor.paramss)
        buf.result()
      }
    }
  }

  implicit class XtensionDefnDef(private val tree: Def) extends AnyVal {
    def `def`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensDef, S.`def`)
    }
    def tokensDef: KwDef = tree.findBefore2[KwDef](_.name)
  }

  implicit class XtensionDefnMacro(private val tree: Macro) extends AnyVal {
    def `def`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensDef, S.`def`)
    }
    def tokensDef: KwDef = tree.findBefore2[KwDef](_.name)
  }

  implicit class XtensionDefnObjectSyntax(private val tree: Object)
      extends AnyVal {
    def `object`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensObject, S.`object`)
    }

    def tokensObject: KwObject = tree.findBefore2[KwObject](_.name)
  }

  implicit class XtensionDefnTraitSyntax(private val tree: Trait)
      extends AnyVal {
    def `trait`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensTrait, S.`trait`)
    }

    def tokensTrait: KwTrait = tree.findBefore2[KwTrait](_.name)
  }

  implicit class XtensionDefnType(private val tree: Type) extends AnyVal {
    def `type`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensType, S.`type`)
    }
    def tokensType: KwType = tree.findBefore2[KwType](_.name)
  }

  implicit class XtensionDefnVal(private val tree: Val) extends AnyVal {
    def `val`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensVal, S.`val`)
    }
    def tokensVal: KwVal = tree.findBefore2[KwVal](_.pats.head)
  }

  implicit class XtensionDefnVar(private val tree: Var) extends AnyVal {
    def `var`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensVar, S.`var`)
    }
    def tokensVar: KwVar = tree.findBefore2[KwVar](_.pats.head)
  }

}
