package scala.meta.internal.prettyprinters
package tokens

import TokensOps._
import Comments._

import scala.meta._
import scala.meta.Token._

import scala.meta.internal.paiges.Doc
import scala.meta.internal.prettyprinters.{ScalaToken => S}

case class ParamSeparator(
  `(`: Doc,
  `,`: List[Doc],
  `)`: Doc
)

case class ParamSeparatorTokens(
    tokenLeftParen: LeftParen,
    tokensComma: List[Comma],
    tokenRightParen: RightParen
) {
  def toParamSeparator(
      tree: Tree,
      withTrailingRightParen: Boolean = false
  )(implicit trivia: AssociatedTrivias): ParamSeparator = {
    val leftParen =
      if (withTrailingRightParen) {
        trivia.wrap(tree, tokenRightParen, S.`)`)
      } else {
        trivia.addLeading(tree, tokenRightParen, S.`)`)
      }

    ParamSeparator(
      trivia.addTrailing(tree, tokenLeftParen, S.`(`),
      tokensComma.map(
        comma => trivia.wrap(tree, comma, S.`,`, isSeparator = true)
      ),
      leftParen
    )
  }
}


object TParamSeparator {
  def empty: TParamSeparator = TParamSeparator(S.`[`, Nil, S.`]`)
}

case class TParamSeparator(
  `[`: Doc,
  `,`: List[Doc],
  `]`: Doc
)

case class TParamSeparatorTokens(
  tokenLeftBrace: LeftBracket,
  tokensComma: List[Comma],
  tokenRightBrace: RightBracket
) {
  def toTParamSeparator(
      tree: Tree
  )(implicit trivia: AssociatedTrivias): TParamSeparator = {
    TParamSeparator(
      trivia.addTrailing(tree, tokenLeftBrace, S.`[`),
      tokensComma.map(
        comma => trivia.wrap(tree, comma, S.`,`, isSeparator = true)
      ),
      trivia.addLeading(tree, tokenRightBrace, S.`]`)
    )
  }
}

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

  implicit class XtensionTermTupleSyntax(private val tree: Term.Tuple)
      extends AnyVal {

    def tokensComma: List[Comma] = commaSeparated(tree)(_.args)
    def tokenLeftParen: LeftParen = tree.find[LeftParen].get
    def tokenRightParen: RightParen =
      tree.findAfter[RightParen](_.args.last).get

    def `,`(implicit trivia: AssociatedTrivias): List[Doc] = {
      tokensComma.map(
        token => trivia.wrap(tree, token, S.`,`, isSeparator = true)
      )
    }

    def `(`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addTrailing(tree, tokenLeftParen, S.`(`)
    def `)`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addLeading(tree, tokenRightParen, S.`)`)

  }

  implicit class XtensionTermFunctionSyntax(private val tree: Term.Function)
      extends AnyVal {
    def tokenRigthArrow: RightArrow =
      if (tree.params.nonEmpty) {
        if (tree.body.tokens.nonEmpty)
          tree.findBetween[RightArrow](_.params.last, _.body).get
        else tree.findAfter[RightArrow](_.params.last).get
      } else tree.find[RightArrow].get

    def tokenLeftParen: Option[LeftParen] =
      if (tree.params.nonEmpty) tree.findBefore[LeftParen](_.params.head)
      else None

    def tokenRightParen: Option[RightParen] =
      if (tree.params.nonEmpty) {
        if (tree.body.tokens.nonEmpty)
          tree.findBetween[RightParen](_.params.last, _.body)
        else tree.findAfter[RightParen](_.params.last)
      } else None

    def tokensComma: List[Comma] = commaSeparated(tree)(_.params)

    def `=>`(implicit trivia: AssociatedTrivias): Doc =
      trivia.wrap(tree, tokenRigthArrow, S.`=>`)
    def `(`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addTrailingOpt(tree, tokenLeftParen, S.`(`)
    def `)`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addLeadingOpt(tree, tokenRightParen, S.`)`)
    def `,`(implicit trivia: AssociatedTrivias): List[Doc] =
      tokensComma.map(
        token => trivia.wrap(tree, token, S.`,`, isSeparator = true)
      )
  }

  implicit class XtensionTermIfSyntax(private val tree: Term.If)
      extends AnyVal {
    def tokenElse: Option[KwElse] = tree.findBetween[KwElse](_.thenp, _.elsep)
    def `else`(implicit trivia: AssociatedTrivias): Doc =
      trivia.wrapOpt(tree, tokenElse, S.`else`)
  }

  implicit class XtensionTermSelectSyntax(private val tree: Term.Select)
      extends AnyVal {
    def tokenDot: Option[Dot] = tree.findBetween[Dot](_.qual, _.name)
    def `.`(implicit trivia: AssociatedTrivias): Doc =
      trivia.wrapOpt(tree, tokenDot, S.`.`)
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
    def tokensLeftBrace: Option[LeftBrace] = blockStartBrace(tree)
    def `{`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addTrailingOpt(tree, tokensLeftBrace, S.`{`)

    def tokensRightBrace: Option[RightBrace] = blockEndBrace(tree)(_.stats)
    def `}`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addLeadingOpt(tree, tokensRightBrace, S.`}`)
  }

  implicit class XtensionTermPartialFunctionSyntax(
      private val tree: Term.PartialFunction
  ) extends AnyVal {

    def tokensLeftBrace: Option[LeftBrace] = tree.find[LeftBrace]

    def `{`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addTrailingOpt(tree, tokensLeftBrace, S.`{`)

    def tokensRightBrace: Option[RightBrace] = {
      tree.tokens.reverse.collectFirst {
        case x: RightBrace => x
      }
    }

    def `}`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addLeadingOpt(tree, tokensRightBrace, S.`}`)
  }

  // early
  // inits
  // {
  // self
  // stats
  // }
  implicit class XtensionTemplateSyntax(private val tree: Template)
      extends AnyVal {
    def tokensLeftBrace: Option[LeftBrace] =
      if (tree.inits.nonEmpty) {
        tree.findAfter[LeftBrace](_.inits.last)
      } else if (tree.early.nonEmpty) {
        tree.findAfter[LeftBrace](_.early.last)
      } else if (tree.self.tokens.nonEmpty) {
        tree.findAfter[LeftBrace](_.self)
      } else if (tree.stats.nonEmpty) {
        tree.findBefore[LeftBrace](_.stats.head)
      } else {
        tree.find[LeftBrace]
      }

    def `{`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addTrailingOpt(tree, tokensLeftBrace, S.`{`)

    def tokensRightBrace: Option[RightBrace] =
      if (tree.stats.nonEmpty) {
        tree.findAfter[RightBrace](_.stats.last)
      } else if (tree.self.tokens.nonEmpty) {
        tree.findAfter[RightBrace](_.self)
      } else if (tree.inits.nonEmpty) {
        tree.findAfter[RightBrace](_.inits.last)
      } else if (tree.early.nonEmpty) {
        tree.findAfter[RightBrace](_.early.last)
      } else {
        tree.find[RightBrace]
      }

    def `}`(implicit trivia: AssociatedTrivias): Doc =
      trivia.addLeadingOpt(tree, tokensRightBrace, S.`}`)

    def `{ }`(implicit trivia: AssociatedTrivias): Doc = {
      def left(l: LeftBrace): Doc = trivia.addTrailing(tree, l, S.`{`)
      def right(r: RightBrace): Doc = trivia.addLeading(tree, r, S.`}`)

      (tokensLeftBrace, tokensRightBrace) match {
        case (Some(l), Some(r)) => left(l) + right(r)
        case (None, Some(r)) => S.`{` + right(r)
        case (Some(l), None) => left(l) + S.`}`
        case (None, None) => Doc.empty
      }
    }

    def tokensExtends: Option[KwExtends] = {
      if (tree.hasTokens && (tree.inits.nonEmpty || tree.early.nonEmpty)) {
        tree.parent.flatMap { p =>
          val tokenList = TokenList(p.tokens)
          tokenList.leading(tree.tokens.head).collectFirst {
            case x: KwExtends => x
          }
        }
      } else None
    }

    def `extends`(implicit trivia: AssociatedTrivias): Doc = {
      val kw = S.`extends`
      tree.parent.map(p => trivia.wrapOpt(p, tokensExtends, kw)).getOrElse(kw)
    }

    def tokensWith: List[KwWith] = {
      separated(tree.tokens, tree.inits, { case x: KwWith => x })
    }

    def `with`(implicit trivia: AssociatedTrivias): List[Doc] = {
      tokensWith.map(t => trivia.wrap(tree, t, S.`with`))
    }

    def tokensWithEarly: Option[KwWith] = {
      if (tree.early.nonEmpty && tree.inits.nonEmpty) {
        tree.findBetween[KwWith](_.early.last, _.inits.head)
      } else None
    }

    def `with (early)`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrapOpt(tree, tokensWithEarly, S.`with`)
    }
  }

  def getTokensParamsSeparator(
      tokens: Tokens,
      paramss: List[List[Tree]]
  ): List[ParamSeparatorTokens] = {
    try {
      val parensBuilder = List.newBuilder[(LeftParen, RightParen)]
      val matching = MatchingParens(tokens)
      val tokenList = TokenList(tokens)

      var left = tokens.collectFirst { case x: LeftParen => x }
      while (left.nonEmpty) {
        val right = matching.close(left.get).get
        parensBuilder += ((left.get, right))
        left = tokenList.collectFirst(right) { case x: LeftParen => x }
      }
      val parens = parensBuilder.result()

      assert(
        parens.size == paramss.filterNot(_.isEmpty).size ||
          parens.size == paramss.size,
        s"$parens != $paramss"
      )

      paramss.zip(parens).map {
        case (params, (left, right)) =>
          val commas = commaSeparated2(tokens, params)
          ParamSeparatorTokens(left, commas, right)
      }
    } catch {
      case ex @ (_: java.util.NoSuchElementException |
          _: java.lang.AssertionError | _: java.lang.AssertionError) => {
        println()
        println()
        println(tokens)
        println()
        throw ex
      }
    }
  }

  def slice(tokens: Tokens, start: Tree, end: Tree): Tokens = {
    (
      tokens.binarySearch(start.tokens.last),
      tokens.binarySearch(end.tokens.head)
    ) match {
      case (Some(s), Some(e)) => tokens.slice(s + 1, e)
      case _ => tokens
    }
  }

  def commaSeparated2(tokens: Tokens, params: List[Tree]): List[Comma] = {
    separated(tokens, params, { case x: Comma => x })
  }

  def separated[T](
      tokens: Tokens,
      params: List[Tree],
      pf: PartialFunction[Token, T]
  ): List[T] = {
    params match {
      case Nil => Nil
      case _ :: Nil => Nil
      case _ =>
        params
          .sliding(2, 1)
          .map {
            case List(l, r) =>
              slice(tokens, l, r).collectFirst(pf).get
          }
          .toList
    }
  }

  implicit class XtensionCtorPrimary(private val tree: Ctor.Primary)
      extends AnyVal {
    def paramsSeparator(
        implicit trivia: AssociatedTrivias
    ): List[ParamSeparator] = {
      val withTrailingRightParen =
        tree.parent
          .map {
            case t: Defn.Trait => t.templ.tokens.nonEmpty
            case c: Defn.Class => c.templ.tokens.nonEmpty
          }
          .getOrElse(false)

      tokensParamsSeparator.map(
        _.toParamSeparator(tree, withTrailingRightParen)
      )
    }

    def tokensParamsSeparator: List[ParamSeparatorTokens] = {
      if (tree.hasTokens && tree.tokens.nonEmpty) {
        val tokenList = TokenList(tree.tokens)
        val start =
          if (tree.mods.nonEmpty) {
            tokenList
              .trailing(tree.mods.last.tokens.last)
              .headOption
              .getOrElse(
                tree.tokens.last
              )
          } else {
            tree.tokens.head
          }
        val end = tree.tokens.last
        if (start != end) {
          val tokens = tree.tokens.slice2(start, end, includeTo = true)
          getTokensParamsSeparator(tokens, tree.paramss)
        } else Nil
      } else Nil
    }
  }

  implicit class XtensionCtorSecondary(private val tree: Ctor.Secondary)
      extends AnyVal {
    def paramsSeparator(
        implicit trivia: AssociatedTrivias
    ): List[ParamSeparator] =
      tokensParamsSeparator.map(_.toParamSeparator(tree))

    def tokensParamsSeparator: List[ParamSeparatorTokens] = {
      if (tree.hasTokens) {
        val tokens = slice(tree.tokens, tree.name, tree.init)
        getTokensParamsSeparator(tokens, tree.paramss)
      } else Nil
    }
  }

  implicit class XtensionDefnMacro(private val tree: Defn.Macro)
      extends AnyVal {
    def paramsSeparator(
        implicit trivia: AssociatedTrivias
    ): List[ParamSeparator] =
      tokensParamsSeparator.map(_.toParamSeparator(tree))

    def tokensParamsSeparator: List[ParamSeparatorTokens] = {
      if (tree.hasTokens) {
        import tree._
        doDef(tokens, name, tparams, paramss, decltpe, body)
      } else Nil
    }
  }

  implicit class XtensionDefnDef(private val tree: Defn.Def) extends AnyVal {
    def paramsSeparator(
        implicit trivia: AssociatedTrivias
    ): List[ParamSeparator] =
      tokensParamsSeparator.map(_.toParamSeparator(tree))

    def tokensParamsSeparator: List[ParamSeparatorTokens] = {
      import tree._
      if (tree.hasTokens) {
        doDef(tokens, name, tparams, paramss, decltpe, body)
      } else Nil
    }
  }

  def doDef(
      tokens: Tokens,
      name: Tree,
      tparams: List[Tree],
      paramss: List[List[Tree]],
      decltpe: Option[Tree],
      body: Tree
  ): List[ParamSeparatorTokens] = {

    val tokenList = TokenList(tokens)

    val start = defStart(name, tparams, tokenList)

    val end =
      if (decltpe.nonEmpty && decltpe.get.tokens.nonEmpty) {
        tokenList.leading(decltpe.get.tokens.head).find(_.is[Colon]).get
      } else {
        val bodyStart = body.tokens.head
        val beforeBody = tokenList.leading(bodyStart)

        beforeBody.find(_.is[RightParen]) match {
          case Some(eq) => tokenList.next(eq)
          case None => tokenList.prev(bodyStart)
        }
      }

    getTokensParamsSeparator(tokens.slice(start, end), paramss)
  }

  def defStart(name: Tree, tparams: List[Tree], tokenList: TokenList): Token = {
    if (tparams.nonEmpty) {
      val bracket = tokenList
        .trailing(tparams.last.tokens.last)
        .find(_.is[RightBracket])
        .get
      val beforeBracket = tokenList.trailing(bracket)
      beforeBracket
        .find(_.is[LeftParen])
        .getOrElse(
          tokenList.next(bracket)
        )
    } else {
      val nameToken = name.tokens.last
      val afterName = tokenList.trailing(nameToken)
      afterName
        .find(_.is[LeftParen])
        .getOrElse(
          tokenList.next(nameToken)
        )
    }
  }

  implicit class XtensionDeclDef(private val tree: Decl.Def) extends AnyVal {
    def paramsSeparator(
        implicit trivia: AssociatedTrivias
    ): List[ParamSeparator] =
      tokensParamsSeparator.map(_.toParamSeparator(tree))

    def tokensParamsSeparator: List[ParamSeparatorTokens] = {
      if (tree.hasTokens && tree.tokens.nonEmpty) {
        val tokenList = TokenList(tree.tokens)

        val start = defStart(tree.name, tree.tparams, tokenList)
        val end = {
          val ts = tree.decltpe.tokens
          if (ts.nonEmpty) {
            tokenList.leading(ts.head).find(_.is[Colon]).get
          } else {
            tree.tokens.last
          }
        }
        if (start != end) {
          val tokens = tree.tokens.slice2(start, end, includeTo = true)
          getTokensParamsSeparator(tokens, tree.paramss)
        } else {
          Nil
        }
      } else Nil
    }
  }

  implicit class XtensionInit(private val tree: Init) extends AnyVal {
    def paramsSeparator(
        implicit trivia: AssociatedTrivias
    ): List[ParamSeparator] =
      tokensParamsSeparator.map(_.toParamSeparator(tree))

    def tokensParamsSeparator: List[ParamSeparatorTokens] = {
      if (tree.hasTokens && tree.tokens.nonEmpty) {
        val tokenList = TokenList(tree.tokens)
        val start = tokenList
          .trailing(tree.tpe.tokens.last)
          .headOption
          .getOrElse(
            tree.tokens.last
          )
        val end = tree.tokens.last

        if (start != end) {
          val tokens = tree.tokens.slice2(start, end, includeTo = true)
          getTokensParamsSeparator(tokens, tree.argss)
        } else Nil
      } else Nil
    }
  }

  implicit class XtensionDefnClass(private val tree: Defn.Class) extends AnyVal {
    def tparamsSeparator(implicit trivia: AssociatedTrivias): TParamSeparator =
      tokensParamSeparator.map(_.toTParamSeparator(tree)).getOrElse(TParamSeparator.empty)

    def tokensParamSeparator: Option[TParamSeparatorTokens] = {
      if (tree.hasTokens && tree.tokens.nonEmpty && tree.tparams.nonEmpty) {
        val right = 
          if (tree.ctor.tokens.nonEmpty) 
            tree.findBetween[RightBracket](_.tparams.last, _.ctor).get
          else if (tree.templ.tokens.nonEmpty)
            tree.findBetween[RightBracket](_.tparams.last, _.templ).get
          else
            tree.findAfter[RightBracket](_.tparams.last).get

        Some(
          TParamSeparatorTokens(
            tree.findBetween[LeftBracket](_.name, _.tparams.head).get,
            commaSeparated2(tree.tokens, tree.tparams),
            right
          )
        )
      } else None
    }
  }

  implicit class XtensionTypeApply(private val tree: Type.Apply) extends AnyVal {
    def tparamsSeparator(implicit trivia: AssociatedTrivias): TParamSeparator =
      tokensParamSeparator.map(_.toTParamSeparator(tree)).getOrElse(TParamSeparator.empty)

    def tokensParamSeparator: Option[TParamSeparatorTokens] = {
      if (tree.hasTokens && tree.tokens.nonEmpty) {
        Some(
          TParamSeparatorTokens(
            tree.findBetween[LeftBracket](_.tpe, _.args.head).get,
            commaSeparated2(tree.tokens, tree.args),
            tree.findAfter[RightBracket](_.args.last).get
          )
        )
      } else None
    }
  }

  implicit class XtensionCase(private val tree: Case) extends AnyVal {
    def tokenRigthArrow: RightArrow = {
      
      val start =
        if (tree.cond.nonEmpty) tree.cond.get.tokens.last
        else tree.pat.tokens.last

      val end = 
        if (tree.body.tokens.nonEmpty) tree.body.tokens.head
        else tree.tokens.last

      tree.tokens.slice2(start, end, includeTo = true).collectFirst{ case x: RightArrow => x }.get
    }

    def `=>`(implicit trivia: AssociatedTrivias): Doc =
      trivia.wrap(tree, tokenRigthArrow, S.`=>`)
  }

  implicit class XtensionDefnVal(private val tree: Defn.Val) extends AnyVal {
    def tokenEquals: Equals = {
      if (tree.decltpe.nonEmpty) tree.findBetween[Equals](_.decltpe.get, _.rhs).get
      else tree.findBetween[Equals](_.pats.head, _.rhs).get
    }

    def `=`(implicit trivia: AssociatedTrivias): Doc =
      trivia.wrap(tree, tokenEquals, S.`=`)
  }
}
