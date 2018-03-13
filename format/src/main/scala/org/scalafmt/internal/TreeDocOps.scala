package org.scalafmt.internal

import org.scalafmt.Options
import scala.meta.Lit
import scala.meta.Mod
import scala.meta.Name
import scala.meta.Pat
import scala.meta.Ref
import scala.meta.Self
import scala.meta.Type
import scala.meta.Term
import scala.meta.Tree

import scala.meta.internal.prettyprinters.DoubleQuotes
import scala.meta.internal.prettyprinters.QuoteStyle
import scala.meta.internal.prettyprinters.TripleQuotes
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc._
import org.langmeta.inputs.Input
import scala.meta.internal.format.CustomTrees._
import org.scalafmt.internal.ScalaToken._
import org.scalafmt.internal.TokenOps._

import scala.meta.internal.fmt.SyntacticGroup.Term._
import scala.meta.internal.fmt.SyntacticGroup.Type._

import scala.language.implicitConversions

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
    print(root)
  }

  def dInfixType(left: Tree, operator: Doc, right: Tree): Doc = {
    val op = operator.render(100)
    val leftWraped = InfixTyp(op).wrap(left)
    val rightWraped = InfixTyp(op).wrap(right, Side.Right)
    leftWraped + space + operator + space + rightWraped
  }

  def dTypeFunction(params: List[Type], res: Type): Doc = {
    val dparams = params match {
      case Nil => `(` + `)`
      case param :: Nil if !param.is[Type.Tuple] =>
        AnyInfixTyp.wrap(param)
      case params => dApplyParen(empty, params)
    }
    dparams + space + `=>` + space + Typ.wrap(res)
  }

  def dName(name: Tree): Doc = name match {
    case _: Name.Anonymous => wildcard
    case _ => print(name)
  }

  def dWithin(keyword: Doc, within: Ref): Doc =
    within match {
      case Name.Anonymous() => keyword
      case Term.This(Name.Anonymous()) => keyword + `[` + `this` + `]`
      case _ => dApplyBracket(keyword, within :: Nil)
    }

  def dApplyBrace(fun: Doc, args: List[Tree]): Doc =
    dApply(fun, args, `{`, `}`)

  def dTargs(targs: List[Tree]): Doc =
    dApplyBracket(empty, targs)

  def dArgs(args: List[Tree]): Doc =
    dApplyParen(empty, args)

  def dArgss(argss: List[List[Term]]): Doc =
    joined(argss.map(dArgs))

  def dApplyParen(fun: Doc, args: List[Tree]): Doc =
    dApply(fun, args, `(`, `)`)

  def dApplyBracket(fun: Doc, args: List[Tree]): Doc =
    if (args.isEmpty) fun
    else dApply(fun, args, `[`, `]`)

  def dApply(fun: Doc, args: List[Tree], left: Doc, right: Doc): Doc = {
    val dargs = intercalate(comma + line, args.map(print))
    dargs.tightBracketBy(fun + left, right)
  }

  def dApplyParenPat(fun: Doc, args: List[Pat]): Doc = {
    val dargs = intercalate(comma + line, args.map(dPat))
    dargs.tightBracketBy(fun + `(`, `)`)
  }

  def dAscription(lhs: Tree, rhs: Tree): Doc = {
    dAscription(lhs, print(rhs))
  }

  def dAscription(lhs: Tree, rhs: Doc): Doc = {
    val dlhs = dName(lhs)
    Ascription.wrap0(lhs, dlhs) + typedColon(dlhs) + space + rhs
  }

  def dBlock(stats: List[Tree]): Doc =
    dBlockI(stats).grouped

  def dBlockI(stats: List[Tree]): Doc = {
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

  def joined(docs: List[Doc]): Doc =
    intercalate(empty, docs)

  def spaceSeparated(docs: List[Doc]): Doc =
    intercalate(space, docs.filterNot(_.isEmpty))

  def commaSeparated(docs: List[Doc]): Doc =
    intercalate(comma + space, docs.filterNot(_.isEmpty))

  def typedColon(lhs: Doc): Doc =
    if (needsLeadingSpaceBeforeColon(lhs.render(100))) space + `:`
    else `:`

  def dMods(mods: List[Mod]): Doc =
    intercalate(space, mods.map(print))

  def dParamss(paramss: List[List[Term.Param]]): Doc =
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

  def dBody(body: Tree): Doc =
    dBodyO(Some(body))

  def dBodyO(body: Option[Tree]): Doc =
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
  ): Doc = {
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
  ): Doc = {
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

  def dStats(stats: List[Tree]): Doc = {
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

  def dRaw(str: String, quoteStyle: QuoteStyle): Doc =
    dRawI(str, 0, Some(quoteStyle))

  def dRawI(str: String, start: Int, quoteStyle: Option[QuoteStyle]): Doc = {
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

  def dQuote(str: String): (QuoteStyle, Doc) =
    if (isMultiline(str)) TripleQuotes -> `"""`
    else DoubleQuotes -> `"`

  def dInterpolate(prefix: Name, parts: List[Tree], args: List[Tree]): Doc = {

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

  def dParams(params: List[Term.Param], forceParens: Boolean): Doc =
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

  def dPat(pat: Tree): Doc =
    print(mkPat(pat))

  def mkPat(pat: Tree): Tree =
    pat match {
      case t: Term.Name if t.value.headOption.exists(_.isLower) =>
        PatName(t.value)
      case _ => pat
    }

  def dPatXml(pat: Pat.Xml): Doc = {
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
