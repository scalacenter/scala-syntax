package org.scalafmt.internal

import scala.annotation.tailrec
import scala.meta.Case
import scala.meta.Ctor
import org.scalafmt.Options
import scala.meta.Decl
import scala.meta.Defn
import scala.meta.Enumerator
import scala.meta.Import
import scala.meta.Token
import scala.meta.Importee
import scala.meta.Importer
import scala.meta.Init
import scala.meta.Lit
import scala.meta.Mod
import scala.meta.Name
import scala.meta.Pat
import scala.meta.Pkg
import scala.meta.Ref
import scala.meta.Self
import scala.meta.Source
import scala.meta.Stat
import scala.meta.Template
import scala.meta.Type
import scala.meta.Term
import scala.meta.Tree
import scala.meta.internal.fmt.SyntacticGroup
import scala.meta.internal.prettyprinters.DoubleQuotes
import scala.meta.internal.prettyprinters.QuoteStyle
import scala.meta.internal.prettyprinters.TripleQuotes
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc._
import org.langmeta.inputs.Input
import scala.meta.internal.format.CustomTrees._
import org.scalafmt.internal.ScalaToken._
import org.scalafmt.internal.TreeOps._
import org.scalafmt.internal.TokenOps._

object TreeDocOps {
  import TreePrinter._

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

  implicit class XtensionSyntacticGroup(val leftGroup: SyntacticGroup)
      extends AnyVal {
    def wrap(tree: Tree, side: Side = Side.Left): Doc = {
      val rightGroup = TreeSyntacticGroup(tree)
      val doc = print(tree)
      if (TreeOps.groupNeedsParens(leftGroup, rightGroup, side)) `(` + doc + `)`
      else doc
    }
  }
  implicit class XtensionTreeDoc(val tree: Tree) extends AnyVal {
    def wrapped: Doc = {
      val doc = print(tree)
      if (needsParens(tree)) wrapParens(doc)
      else doc
    }
  }

  def wrapParens(doc: Doc): Doc = `(` + doc + `)`

  def isRightAssociative(op: String): Boolean = op.endsWith(":")

  object LambdaArg {
    type Paramss = Vector[List[Term.Param]]

    @tailrec
    private final def getParamss(
        f: Term.Function,
        accum: Paramss = Vector.empty
    ): (Paramss, Term) =
      f.body match {
        case g: Term.Function =>
          getParamss(g, accum :+ f.params)
        case Term.Block((g: Term.Function) :: Nil) =>
          getParamss(g, accum :+ f.params)
        case _ =>
          (accum :+ f.params, f.body)
      }

    def dFunction(f: Term.Function): Doc = {
      val (paramss, body) = getParamss(f)
      val dbody = body match {
        case Term.Block(stats) => dStats(stats)
        case _ => print(body)
      }
      val dparamss = paramss.foldLeft(empty) {
        case (accum, params) =>
          accum + line + dParams(params, forceParens = false) + space + `=>`
      }
      val result = `{` +
        (dparamss.nested(2).grouped + line + dbody).nested(2).grouped +
        line + `}`
      result.grouped
    }

    def unapply(args: List[Tree]): Option[Doc] =
      args match {
        case (arg: Term.PartialFunction) :: Nil =>
          Some(print(arg))
        case (arg @ Term.Function(_, Term.Block(_ :: _ :: _))) :: Nil =>
          Some(dFunction(arg))
        case (Term.Block((f: Term.Function) :: Nil)) :: Nil =>
          Some(dFunction(f))
        case _ =>
          None
      }
  }

  // TODO(olafur) verify that different precedence of type/term infix operators
  // does not affect this method:
  // https://docs.scala-lang.org/sips/make-types-behave-like-expressions.html
  def dInfix(
      lhs: Tree,
      op: String,
      opDoc: Doc,
      args: List[Tree]
  ): Doc = {
    val opPrecedence = operatorPrecedence(op)
    // TODO(olafur) generalize over lhs/rhs comparison, there is so much
    // duplication between the two cases.
    val dlhs: Doc = lhs match {
      case arg @ Term.ApplyInfix(_, Term.Name(lop), _, _) =>
        val darg = print(arg)
        val leftPrecedence = operatorPrecedence(lop)
        if (isRightAssociative(op)) {
          if (!isRightAssociative(lop)) wrapParens(darg)
          else if (leftPrecedence >= opPrecedence) wrapParens(darg)
          else darg
        } else {
          if (isRightAssociative(lop)) wrapParens(darg)
          else if (leftPrecedence < opPrecedence) wrapParens(darg)
          else darg
        }
      case _ =>
        lhs.wrapped
    }
    val dargs: Doc = args match {
      case LambdaArg(doc) => doc
      case Lit.Unit() :: Nil => `(` + `(` + `)` + `)`
      case Term.Block(stats) :: Nil => dBlock(stats)
      case (arg @ Infix(rop)) :: Nil =>
        val rightPrecedence = operatorPrecedence(rop)
        val darg = print(arg)
        if (isRightAssociative(op)) {
          if (!isRightAssociative(rop)) wrapParens(darg)
          else if (rightPrecedence > opPrecedence) wrapParens(darg)
          else darg
        } else {
          if (rightPrecedence <= opPrecedence) wrapParens(darg)
          else darg
        }
      case arg :: Nil =>
        if (needsParens(arg)) {
          dApplyParen(empty, args)
        } else {
          print(arg)
        }
      case _ => dApplyParen(empty, args)
    }
    dlhs + space + opDoc + space + dargs
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

  // This is a quick hack to prevent unnecessary parens.
  def dPath(lhs: Tree, lhsDoc: Doc, sep: Doc, rhs: Doc): Doc = {
    if (!needsParens(lhs)) lhsDoc + sep + rhs
    else `(` + lhsDoc + `)` + sep + rhs
  }

  def dTyped(lhs: Tree, rhs: Tree): Doc = {
    dTyped(lhs, print(rhs))
  }

  def dTyped(lhs: Tree, rhs: Doc): Doc = {
    val dlhs = dName(lhs)
    if (needsParens(lhs)) wrapParens(dlhs) + `:` + space + rhs
    else dlhs + typedColon(dlhs) + space + rhs
  }

  def dBlock(stats: List[Tree]): Doc =
    dBlockI(stats).grouped

  def dBlockI(stats: List[Tree]): Doc = stats match {
    case Nil => `{` + `}`
    case head :: Nil =>
      `{` + ((line + print(head)).nested(2) + line) + `}`
    case _ =>
      `{` +
        (line + dStats(stats)).nested(2) +
        line + `}`
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
      case Nil :: Nil => `(` + `)`
      case _ =>
        joined(paramss.map { params =>
          val dimplicit =
            if (params.exists(_.mods.exists(_.is[Mod.Implicit])))
              `implicit` + line
            else empty
          val dparams = params.map { param =>
            print(param.copy(mods = param.mods.filterNot(_.is[Mod.Implicit])))
          }
          (dimplicit + intercalate(comma + line, dparams))
            .tightBracketBy(`(`, `)`)
        })
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
    intercalate(lineBlank, stats.map {
      case t: Term.Xml => wrapParens(print(t))
      case t => print(t)
    })
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

    def escape(part: String) =
      dRawI(part.replace("$", "$$"), 0, None)

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
          case Some(tpe) if forceParens || needsParens(tpe) =>
            `(` + dparam + `)`
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
}
