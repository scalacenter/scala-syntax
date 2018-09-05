package scala.meta.internal.prettyprinters
package tokens

import scala.meta.internal.prettyprinters.{ScalaToken => S}

import scala.meta._
import scala.meta.tokens.Token._

import scala.meta.internal.paiges.Doc

object SyntaxTokensDecl {
  import Decl._
  import SyntaxTokensUtils._

  implicit class XtensionDeclDef(private val tree: Def) extends AnyVal {
    def `def`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensDef, S.`def`)
    }
    def tokensDef: KwDef = tree.findBefore2[KwDef](_.name)
  }

  implicit class XtensionDeclType(private val tree: Type) extends AnyVal {
    def `type`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensType, S.`type`)
    }
    def tokensType: KwType = tree.findBefore2[KwType](_.name)
  }

  implicit class XtensionDeclVal(private val tree: Val) extends AnyVal {
    def `val`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensVal, S.`val`)
    }
    def tokensVal: KwVal = tree.findBefore2[KwVal](_.pats.head)
  }

  implicit class XtensionDeclVar(private val tree: Var) extends AnyVal {
    def `var`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensVar, S.`var`)
    }
    def tokensVar: KwVar = tree.findBefore2[KwVar](_.pats.head)
  }

}
