package scala.meta.internal.prettyprinters
package tokens

import scala.meta._
import scala.meta.Token._

import scala.meta.internal.paiges.Doc
import scala.meta.internal.prettyprinters.{ScalaToken => S}

object SyntaxTokens {

  import SyntaxTokensUtils._

  def commas[T <: Tree](
      tokensComma: List[List[Comma]],
      tree: T,
      trivia: AssociatedTrivias
  ): List[List[Doc]] =
    tokensComma.map(
      _.map(token => trivia.wrap(tree, token, S.`,`, isSeparator = true))
    )

  implicit class XtensionTermApplySyntax(private val tree: Term.Apply)
      extends AnyVal {
    def tokensComma: List[Comma] = commaSeparated(tree)(_.args)
    def `,`(implicit trivia: AssociatedTrivias): List[Doc] = {
      tokensComma.map(
        token => trivia.wrap(tree, token, S.`,`, isSeparator = true)
      )
    }
  }

  implicit class XtensionTermBlockSyntax(private val tree: Term.Block)
      extends AnyVal {
    def tokensLeftBrace: LeftBrace = blockStartBrace(tree)
    def `{`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addTrailing(tree, tokensLeftBrace, S.`{`)

    def tokensRightBrace: RightBrace = blockEndBrace(tree)(_.stats)
    def `}`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addLeading(tree, tokensRightBrace, S.`}`)
  }

  implicit class XtensionCtorPrimary(private val tree: Ctor.Primary)
      extends AnyVal {
    def tokensComma: List[List[Comma]] = tree.paramss.map(commaSeparated0(tree))
    def `,`(implicit trivia: AssociatedTrivias): List[List[Doc]] =
      commas(tokensComma, tree, trivia)
  }

  implicit class XtensionCtorSecondary(private val tree: Ctor.Secondary)
      extends AnyVal {
    def tokensComma: List[List[Comma]] = tree.paramss.map(commaSeparated0(tree))
    def `,`(implicit trivia: AssociatedTrivias): List[List[Doc]] =
      commas(tokensComma, tree, trivia)
  }

  implicit class XtensionDeclDef(private val tree: Decl.Def) extends AnyVal {
    def tokensComma: List[List[Comma]] = tree.paramss.map(commaSeparated0(tree))
    def `,`(implicit trivia: AssociatedTrivias): List[List[Doc]] =
      commas(tokensComma, tree, trivia)
  }
  implicit class XtensionDefnMacro(private val tree: Defn.Macro)
      extends AnyVal {
    def tokensComma: List[List[Comma]] = tree.paramss.map(commaSeparated0(tree))
    def `,`(implicit trivia: AssociatedTrivias): List[List[Doc]] =
      commas(tokensComma, tree, trivia)
  }
  implicit class XtensionDefnDef(private val tree: Defn.Def) extends AnyVal {
    def tokensComma: List[List[Comma]] = tree.paramss.map(commaSeparated0(tree))
    def `,`(implicit trivia: AssociatedTrivias): List[List[Doc]] =
      commas(tokensComma, tree, trivia)
  }
}
