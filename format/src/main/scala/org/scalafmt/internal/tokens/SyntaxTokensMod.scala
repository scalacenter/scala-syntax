package org.scalafmt.internal
package tokens

import org.scalafmt.internal.{ScalaToken => S}

import scala.meta._
import scala.meta.tokens.Token._

import org.typelevel.paiges.Doc

object SyntaxTokensMod {
  import Mod._
  import SyntaxTokensUtils._

  implicit class XtensionModAnnotSyntax(private val tree: Annot)
      extends AnyVal {
    def `@`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensAt, S.`@`)
    }
    def tokensAt: At = tree.find[At].get
  }

  implicit class XtensionModPrivateSyntax(private val tree: Private)
      extends AnyVal {
    def `private`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensPrivate, S.`private`)
    }
    def tokensPrivate: KwPrivate = tree.find[KwPrivate].get
  }

  implicit class XtensionModProtectedSyntax(private val tree: Protected)
      extends AnyVal {
    def `protected`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensProtected, S.`protected`)
    }
    def tokensProtected: KwProtected = tree.find[KwProtected].get
  }

}
