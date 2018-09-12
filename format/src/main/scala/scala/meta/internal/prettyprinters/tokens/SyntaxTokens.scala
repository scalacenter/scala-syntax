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

  implicit class XtensionTermTupleSyntax(private val tree: Term.Tuple) extends AnyVal {
    
    def tokensComma: List[Comma] = commaSeparated(tree)(_.args)
    def tokenLeftParen: LeftParen = tree.find[LeftParen].get
    def tokenRightParen: RightParen = tree.findAfter[RightParen](_.args.last).get

    def `,`(implicit trivia: AssociatedTrivias): List[Doc] = {
      tokensComma.map(
        token => trivia.wrap(tree, token, S.`,`, isSeparator = true)
      )
    }

    def `(`(implicit trivia: AssociatedTrivias): Doc = 
      trivia.wrap(tree, tokenLeftParen, S.`(`)
    def `)`(implicit trivia: AssociatedTrivias): Doc = 
      trivia.wrap(tree, tokenRightParen, S.`)`)

  }

  implicit class XtensionTermFunctionSyntax(private val tree: Term.Function) extends AnyVal {
    def tokenRigthArrow: RightArrow = 
      if (tree.params.nonEmpty) tree.findBetween[RightArrow](_.params.last, _.body).get
      else tree.find[RightArrow].get
    def `=>`(implicit trivia: AssociatedTrivias): Doc = 
      trivia.wrap(tree, tokenRigthArrow, S.`=>`)
  }

  implicit class XtensionTermIfSyntax(private val tree: Term.If)
      extends AnyVal {
    def tokenElse: Option[KwElse] = tree.findBetween[KwElse](_.thenp, _.elsep)
    def `else`(implicit trivia: AssociatedTrivias): Doc =
      trivia.wrapOpt(tree, tokenElse, S.`else`)
  }

  implicit class XtensionTermSelectSyntax(private val tree: Term.Select)
      extends AnyVal {
    def tokenDot: Dot =
      tree.findBetween[Dot](_.qual, _.name).get
    def `.`(implicit trivia: AssociatedTrivias): Doc =
      trivia.wrap(tree, tokenDot, S.`.`)
  }

  implicit class XtensionTermApplySyntax(private val tree: Term.Apply)
      extends AnyVal {
    def tokensComma: List[Comma] = commaSeparated(tree)(_.args)
    def `,`(implicit trivia: AssociatedTrivias): List[Doc] = {
      tokensComma.map(
        token => trivia.wrap(tree, token, S.`,`, isSeparator = true)
      )
    }

    def tokensLeftParen: LeftParen = tree.findAfter[LeftParen](_.fun).get
    def `(`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addTrailing(tree, tokensLeftParen, S.`(`)

    def tokensRightParen: RightParen =
      if (tree.args.nonEmpty) tree.findAfter[RightParen](_.args.last).get
      else tree.findAfter[RightParen](_.fun).get

    def `)`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addLeading(tree, tokensRightParen, S.`)`)

    def tokensLeftBrace: LeftBrace = tree.findAfter[LeftBrace](_.fun).get
    def `{`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addTrailing(tree, tokensLeftBrace, S.`{`)
    def tokensRightBrace: RightBrace = {
      if (tree.args.nonEmpty) tree.args.head.tokens.reverse.collectFirst {
        case r: RightBrace => r
      }.get
      else tree.findAfter[RightBrace](_.fun).get
    }
    def `}`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addLeading(tree, tokensRightBrace, S.`}`)
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

  // early
  // inits
  // {
  // self
  // stats
  // }
  implicit class XtensionTemplateSyntax(private val templ: Template)
      extends AnyVal {
    def tokensLeftBrace: Option[LeftBrace] =
      if (templ.inits.nonEmpty) {
        templ.findAfter[LeftBrace](_.inits.last)
      } else if (templ.early.nonEmpty) {
        templ.findAfter[LeftBrace](_.early.last)
      } else if (templ.self.tokens.nonEmpty) {
        templ.findAfter[LeftBrace](_.self)
      } else if (templ.stats.nonEmpty) {
        templ.findBefore[LeftBrace](_.stats.head)
      } else {
        templ.find[LeftBrace]
      }

    def `{`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addTrailingOpt(templ, tokensLeftBrace, S.`{`)

    def tokensRightBrace: Option[RightBrace] =
      if (templ.stats.nonEmpty) {
        templ.findAfter[RightBrace](_.stats.last)
      } else if (templ.self.tokens.nonEmpty) {
        templ.findAfter[RightBrace](_.self)
      } else if (templ.inits.nonEmpty) {
        templ.findAfter[RightBrace](_.inits.last)
      } else if (templ.early.nonEmpty) {
        templ.findAfter[RightBrace](_.early.last)
      } else {
        templ.find[RightBrace]
      }

    def `}`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addLeadingOpt(templ, tokensRightBrace, S.`}`)

    def `{ }`(implicit trivia: AssociatedTrivias): Doc = {
      def left(l: LeftBrace): Doc = trivia.addTrailing(templ, l, S.`{`)
      def right(r: RightBrace): Doc = trivia.addLeading(templ, r, S.`}`)

      (tokensLeftBrace, tokensRightBrace) match {
        case (Some(l), Some(r)) => left(l) + right(r)
        case (None, Some(r)) => S.`{` + right(r)
        case (Some(l), None) => left(l) + S.`}`
        case (None, None) => Doc.empty
      }
    }
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
