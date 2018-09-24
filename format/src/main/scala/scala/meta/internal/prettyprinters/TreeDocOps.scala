package scala.meta.internal.prettyprinters

import tokens.SyntaxTokens._
import tokens.ParamSeparator

import SyntacticGroup.Term._
import SyntacticGroup.Type._
import ScalaToken._
import TokenOps._
import CustomTrees._

import scala.meta.internal.prettyprinters.{ScalaToken => S}

import scala.meta.internal.paiges.Doc
import scala.meta.internal.paiges.Doc._

import scala.meta._
import scala.meta.internal.prettyprinters._

import scala.annotation.tailrec

trait TreeDocOps extends SyntacticGroupOps with TreePrinterUtils {
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
      case params => dApplyParen(empty, `(`, params, Nil, `)`)
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

  def dArgs(`(`: Doc, args: List[Tree], `,`: List[Doc], `)`: Doc): Doc =
    dApplyParen(empty, `(`, args, `,`, `)`)

  def dApplyParen(
      fun: Doc,
      `(`: Doc,
      args: List[Tree],
      `,`: List[Doc],
      `)`: Doc
  ): Doc =
    dApply(fun, `(`, args, `,`, `)`)

  def dApplyBracket(fun: Doc, args: List[Tree]): Doc =
    if (args.isEmpty) fun
    else dApply(fun, args, `[`, `]`)

  def dApplyBracket(
      fun: Doc,
      args: List[Tree],
      tparamSeparator: tokens.TParamSeparator
  ): Doc =
    if (args.isEmpty) fun
    else {
      import tparamSeparator._
      dApply(fun, `[`, args, `,`, `]`)
    }

  def dApply(fun: Doc, args: List[Tree], left: Doc, right: Doc): Doc = {
    val dargs = intercalate(comma + line, args.map(print))
    dargs.tightBracketBy(fun + left, right)
  }

  def dApply(
      fun: Doc,
      `(`: Doc,
      args: List[Tree],
      `,`: List[Doc],
      `)`: Doc
  ): Doc = {
    args.mkDoc(`,`).tightBracketBy(fun + `(`, `)`)
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

  def dBlock(`{`: Doc, stats: List[Tree], `}`: Doc): Doc =
    dBlockI(`{`, stats, `}`).grouped

  def dBlockI(stats: List[Tree]): Doc =
    dBlockI(`{`, stats, `}`)

  def dBlockI(`{`: Doc, stats: List[Tree], `}`: Doc): Doc = {
    val hasTrailingComment =
      stats.lastOption.map(trivia.hasTrailingComment).getOrElse(false)

    val nl =
      if (hasTrailingComment) empty
      else line

    val body =
      stats match {
        case Nil =>
          empty
        case head :: Nil =>
          (line + print(head)).nested(2) + nl
        case _ =>
          (line + dStats(stats)).nested(2) + nl
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

  def dParamss(
      paramss: List[List[Term.Param]],
      separators: List[tokens.ParamSeparator]
  ): Doc =
    paramss match {
      case Nil => empty
      case List(Nil) => {
        separators.headOption.map(sep => sep.`(` + sep.`)`).getOrElse(`(` + `)`)
      }
      case _ => {
        val printedParams =
          paramss.map { params =>
            if (params.nonEmpty) {
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
              (dimplicit + dparams.head) :: dparams.tail
            } else Nil
          }

        joined(printedParams, separators)
      }
    }

  def joined(
      docss: List[List[Doc]],
      separators: List[tokens.ParamSeparator]
  ): Doc = {
    val separators0 =
      if (separators.isEmpty) {
        docss.map(
          docs => ParamSeparator(`(`, docs.map(_ => `,` + space).drop(1), `)`)
        )
      } else separators

    assert(docss.size == separators0.size)
    cat(docss.zip(separators0).map {
      case (docs, separator) =>
        if (docs.nonEmpty) {
          assert(
            docs.size == separator.`,`.size + 1 ||
              docs.size == separator.`,`.size
          )
        }

        docs
          .zipAll(separator.`,`, empty, empty)
          .foldLeft(empty) {
            case (acc, (term, sep)) => {
              acc + term + sep
            }
          }
          .tightBracketBy(separator.`(`, separator.`)`)
    })
  }

  def dBody(body: Tree, `=`: Doc = S.`=`): Doc =
    dBodyO(Some(body), `=`)

  def dBodyO(body: Option[Tree], `=`: Doc = S.`=`): Doc =
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
      paramssSeparators: List[tokens.ParamSeparator],
      decltpe: Option[Type],
      body: Doc
  ): Doc = {
    val dname = commaSeparated(pats.map(print))
    dDef(mods, keyword, dname, tparams, paramss, paramssSeparators, decltpe,
      body)
  }

  def dDef(
      mods: List[Mod],
      keyword: Doc,
      name: Doc,
      tparams: List[Type.Param],
      paramss: List[List[Term.Param]],
      paramssSeparators: List[tokens.ParamSeparator],
      decltpe: Option[Type] = None,
      dbody: Doc = empty
  ): Doc = {
    val dname = dApplyBracket(name, tparams)
    val dparamss = dParamss(paramss, paramssSeparators)
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

  def dParams(
      `(`: Doc,
      params: List[Term.Param],
      `,`: List[Doc],
      `)`: Doc,
      forceParens: Boolean
  ): Doc = {

    params match {
      case param :: Nil =>
        val dparam = print(param)
        param.decltpe match {
          case Some(tpe) =>
            if (forceParens) wrapParens(`(`, dparam, `)`)
            else SimpleExpr.wrap0(tpe, dparam)
          case _ => dparam
        }
      case _ => dApplyParen(empty, `(`, params, `,`, `)`)
    }
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

  object LambdaArg {
    private def doParams(f: Term.Function): Doc = {
      dParams(f.`(`, f.params, f.`,`, f.`)`, forceParens = false) + space + f.`=>`
    }

    private def dFunction(b: Term.Block, f: Term.Function): Doc = {
      def loop(f0: Term.Function): Doc = {
        val next =
          f0.body match {
            case f1: Term.Function => loop(f1)
            case Term.Block((f1: Term.Function) :: Nil) => loop(f1)
            case body @ Term.Block(stats) => dStats(stats)
            case body => print(body)
          }

        (doParams(f0) + line + next).grouped
      }

      b.`{` + space + loop(f).nested(2) + line + b.`}`
    }

    def unapply(args: List[Tree]): Option[Doc] = {
      args match {
        case (arg: Term.PartialFunction) :: Nil =>
          Some(print(arg))

        case (arg @ Term.Function(_, b @ Term.Block(_ :: _ :: _))) :: Nil =>
          Some(dFunction(b, arg))

        case (b @ Term.Block((f: Term.Function) :: Nil)) :: Nil =>
          Some(dFunction(b, f))

        case _ =>
          None
      }
    }
  }
}
