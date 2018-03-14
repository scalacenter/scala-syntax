package org.scalafmt.internal

import org.scalafmt.internal.{AssociatedTrivias => Trivias}
import org.scalafmt.internal.ScalaToken._
import org.scalafmt.internal.TokenOps._
import org.scalafmt.Options

import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc._

import scala.language.implicitConversions

import scala.meta.internal.fmt.SyntacticGroup.Term._
import scala.meta.internal.fmt.SyntacticGroup.Type._
import scala.meta.internal.format.CustomTrees._
import scala.meta.internal.prettyprinters.DoubleQuotes
import scala.meta.internal.prettyprinters.QuoteStyle
import scala.meta.internal.prettyprinters.TripleQuotes
import scala.meta.Input
import scala.meta.{Lit, Mod, Name, Pat, Ref, Self, Term, Tree, Type}

object TreeDocOps {
  import TreePrinter._
  import SyntacticGroupOps._

  implicit def toDoc(quote: QuoteStyle): Doc = text(quote.toString)

  def getRoot(input: String, options: Options): Tree = {
    getRoot(Input.String(input), options)
  }

  def getRoot(input: Input, options: Options): Tree = {
    options.parser.apply(input, options.dialect).get
  }

  def printInput(input: Input, options: Options): Doc = {
    printTree(getRoot(input, options), options)
  }

  def printTree(root: Tree, options: Options): Doc = {
    TreePrinter.printTree(root)
  }

  def dInfixType(left: Tree, operator: Doc, right: Tree)(
      implicit trivias: Trivias
  ): Doc = {
    val op = operator.render(100)
    val leftWraped = InfixTyp(op).wrap(left)
    val rightWraped = InfixTyp(op).wrap(right, Side.Right)
    leftWraped + space + operator + space + rightWraped
  }

  def dTypeFunction(params: List[Type], res: Type)(
      implicit trivias: Trivias
  ): Doc = {
    val dparams = params match {
      case Nil => `(` + `)`
      case param :: Nil if !param.is[Type.Tuple] =>
        AnyInfixTyp.wrap(param)
      case params => dApplyParen(empty, params)
    }
    dparams + space + `=>` + space + Typ.wrap(res)
  }

  def dName(name: Tree)(implicit trivias: Trivias): Doc = name match {
    case _: Name.Anonymous => wildcard
    case _ => print(name)
  }

  def dWithin(keyword: Doc, within: Ref)(implicit trivias: Trivias): Doc =
    within match {
      case Name.Anonymous() => keyword
      case Term.This(Name.Anonymous()) => keyword + `[` + `this` + `]`
      case _ => dApplyBracket(keyword, within :: Nil)
    }

  def dApplyBrace(fun: Doc, args: List[Tree])(implicit trivias: Trivias): Doc =
    dApply(fun, args, `{`, `}`)

  def dTargs(targs: List[Tree])(implicit trivias: Trivias): Doc =
    dApplyBracket(empty, targs)

  def dArgs(args: List[Tree])(implicit trivias: Trivias): Doc =
    dApplyParen(empty, args)

  def dArgss(argss: List[List[Term]])(implicit trivias: Trivias): Doc =
    joined(argss.map(dArgs))

  def dApplyParen(fun: Doc, args: List[Tree])(implicit trivias: Trivias): Doc =
    dApply(fun, args, `(`, `)`)

  def dApplyBracket(fun: Doc, args: List[Tree])(
      implicit trivias: Trivias
  ): Doc =
    if (args.isEmpty) fun
    else dApply(fun, args, `[`, `]`)

  def dApply(fun: Doc, args: List[Tree], left: Doc, right: Doc)(
      implicit trivias: Trivias
  ): Doc = {
    val dargs = intercalate(comma + line, args.map(print))
    dargs.tightBracketBy(fun + left, right)
  }

  def dApplyParenPat(fun: Doc, args: List[Pat])(
      implicit trivias: Trivias
  ): Doc = {
    val dargs = intercalate(comma + line, args.map(dPat))
    dargs.tightBracketBy(fun + `(`, `)`)
  }

  def dAscription(lhs: Tree, rhs: Tree)(implicit trivias: Trivias): Doc = {
    dAscription(lhs, print(rhs))
  }

  def dAscription(lhs: Tree, rhs: Doc)(implicit trivias: Trivias): Doc = {
    val dlhs = dName(lhs)
    Ascription.wrap0(lhs, dlhs) + typedColon(dlhs) + space + rhs
  }

  def dBlock(stats: List[Tree])(implicit trivias: Trivias): Doc =
    dBlockI(stats).grouped

  def dBlockI(stats: List[Tree])(implicit trivias: Trivias): Doc = {
    val body =
      stats match {
        case Nil =>
          empty
        case head :: Nil =>
          (line + print(head)).nested(2) + line
        case _ =>
          (line + dStats(stats)).nested(2) + line
      }

    `{` + body + `}`
  }

  def isEmpty(self: Self): Boolean = self match {
    case Self(Name.Anonymous(), None) => true
    case _ => false
  }

  def joined(docs: List[Doc])(implicit trivias: Trivias): Doc =
    intercalate(empty, docs)

  def spaceSeparated(docs: List[Doc])(implicit trivias: Trivias): Doc =
    intercalate(space, docs.filterNot(_.isEmpty))

  def commaSeparated(docs: List[Doc])(implicit trivias: Trivias): Doc =
    intercalate(comma + space, docs.filterNot(_.isEmpty))

  def typedColon(lhs: Doc)(implicit trivias: Trivias): Doc =
    if (needsLeadingSpaceBeforeColon(lhs.render(100))) space + `:`
    else `:`

  def dMods(mods: List[Mod])(implicit trivias: Trivias): Doc =
    intercalate(space, mods.map(print))

  def dParamss(
      paramss: List[List[Term.Param]]
  )(implicit trivias: Trivias): Doc =
    paramss match {
      case Nil => empty
      case List(Nil) => `(` + `)`
      case _ => {
        val printedParams =
          paramss.map { params =>
            val dimplicit =
              if (params.exists(_.mods.exists(_.is[Mod.Implicit])))
                `implicit` + line
              else empty

            val dparams =
              params.map { param =>
                print(
                  param.copy(mods = param.mods.filterNot(_.is[Mod.Implicit]))
                )
              }

            (dimplicit + intercalate(comma + line, dparams))
              .tightBracketBy(`(`, `)`)
          }

        joined(printedParams)
      }
    }

  def dBody(body: Tree)(implicit trivias: Trivias): Doc =
    dBodyO(Some(body))

  def dBodyO(body: Option[Tree])(implicit trivias: Trivias): Doc =
    body.fold(empty) {
      case t @ (_: Term.Block | _: Term.PartialFunction | _: Term.Match) =>
        `=` + space + print(t)
      case t =>
        `=` + (line + print(t)).nested(2).grouped
    }

  def dDefPats(
      mods: List[Mod],
      keyword: Doc,
      pats: List[Pat],
      tparams: List[Type.Param],
      paramss: List[List[Term.Param]],
      decltpe: Option[Type],
      body: Doc
  )(implicit trivias: Trivias): Doc = {
    val dname = commaSeparated(pats.map(print))
    dDef(mods, keyword, dname, tparams, paramss, decltpe, body)
  }

  def dDef(
      mods: List[Mod],
      keyword: Doc,
      name: Doc,
      tparams: List[Type.Param],
      paramss: List[List[Term.Param]],
      decltpe: Option[Type] = None,
      dbody: Doc = empty
  )(implicit trivias: Trivias): Doc = {
    val dname = dApplyBracket(name, tparams)
    val dparamss = dParamss(paramss)
    val ddecltpe =
      decltpe.fold(empty)(tpe => typedColon(name) + space + print(tpe))
    spaceSeparated(
      dMods(mods) ::
        keyword ::
        dname + dparamss + ddecltpe ::
        dbody ::
        Nil
    )
  }

  def dStats(stats: List[Tree])(implicit trivias: Trivias): Doc = {
    intercalate(
      lineBlank,
      stats.map {
        // Term.Xml is SimpleExpr1 but binds weaker when with other xml expressions
        // ex: { val x = <a></a>; (<b></b>) }
        case t: Term.Xml => wrapParens(print(t))
        case t => Expr1.wrap(t)
      }
    )
  }

  def dRaw(str: String, quoteStyle: QuoteStyle)(
      implicit trivias: Trivias
  ): Doc =
    dRawI(str, 0, Some(quoteStyle))

  def dRawI(str: String, start: Int, quoteStyle: Option[QuoteStyle])(
      implicit trivias: Trivias
  ): Doc = {
    if (start >= str.length) empty
    else {
      val idx = str.indexOf('\n', start)
      if (idx < 0) {
        val substr = str.substring(start)
        val escaped =
          quoteStyle.fold(substr)(style => SyntaxOps.escape(substr, style))
        text(escaped)
      } else {
        text(str.substring(start, idx)) +
          lineNoFlatNoIndent +
          dRawI(str, idx + 1, quoteStyle)
      }
    }
  }

  def isMultiline(part: String): Boolean = {
    part.contains("\n") &&
    !part.contains("\"\"\"")
  }

  def dQuote(str: String)(implicit trivias: Trivias): (QuoteStyle, Doc) =
    if (isMultiline(str)) TripleQuotes -> `"""`
    else DoubleQuotes -> `"`

  def dInterpolate(prefix: Name, parts: List[Tree], args: List[Tree])(
      implicit trivias: Trivias
  ): Doc = {

    def isMultilineInterpolated(part: String): Boolean =
      // NOTE(olafur) interpolated strings are unescaped so single quotes must
      // be wrapped in triple quote interpolated strings.
      part.contains("\"") || isMultiline(part)

    val isTripleQuoted = parts.exists {
      case Lit.String(part) => isMultilineInterpolated(part)
    }

    def escape(part: String) = {
      val dollar = "$"
      dRawI(part.replace(dollar, dollar + dollar), 0, None)
    }

    val dquote = if (isTripleQuoted) `"""` else `"`
    val dhead = parts.head match {
      case Lit.String(value) =>
        escape(value)
    }
    val sparts = parts.tail.zip(args).foldLeft(empty) {
      case (accum, (Lit.String(part), name: Term.Name))
          if !isIdentifierStart(part) &&
            !name.value.startsWith("_") &&
            !Identifier.needsBacktick(name.value) =>
        accum + `$` + print(name) + escape(part)
      case (accum, (Lit.String(part), arg)) =>
        accum + dApplyBrace(`$`, arg :: Nil) + escape(part)
    }
    print(prefix) + dquote + dhead + sparts + dquote
  }

  def dParams(params: List[Term.Param], forceParens: Boolean)(
      implicit trivias: Trivias
  ): Doc =
    params match {
      case param :: Nil =>
        val dparam = print(param)
        param.decltpe match {
          case Some(tpe) =>
            if (forceParens) wrapParens(dparam)
            else SimpleExpr.wrap0(tpe, dparam)
          case _ => dparam
        }
      case _ => dApplyParen(empty, params)
    }

  def dPat(pat: Tree)(implicit trivias: Trivias): Doc =
    print(mkPat(pat))

  def mkPat(pat: Tree)(implicit trivias: Trivias): Tree =
    pat match {
      case t: Term.Name if t.value.headOption.exists(_.isLower) =>
        PatName(t.value)
      case _ => pat
    }

  def dPatXml(pat: Pat.Xml)(implicit trivias: Trivias): Doc = {
    val parts = pat.parts.map(Some(_))
    val args = pat.args.map(Some(_))
    parts.zipAll(args, None, None).foldLeft(empty) {
      case (acc, (part, argument)) => {
        val printedPart =
          part match {
            case Some(p: Lit.String) => {
              val (quoteStyle, _) = dQuote(p.value)
              dRaw(p.value, quoteStyle)
            }
            // $COVERAGE-OFF$
            case Some(_) =>
              sys.error("xml part expect Lit.String: " + part.structure)
            case _ => empty // impossible, parts.size = args.size + 1
            // $COVERAGE-ON$
          }

        val printedArgument =
          argument.map(a => `{` + print(a) + `}`).getOrElse(empty)

        acc + printedPart + printedArgument
      }
    }
  }
}
