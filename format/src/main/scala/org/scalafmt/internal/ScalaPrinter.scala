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
import scala.meta.internal.prettyprinters.DoubleQuotes
import scala.meta.internal.prettyprinters.QuoteStyle
import scala.meta.internal.prettyprinters.TripleQuotes
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc._
import org.langmeta.inputs.Input
import scala.meta.internal.format.FormatTree._
import org.scalafmt.internal.ScalaToken._
import org.scalafmt.internal.TreeOps._
import org.scalafmt.internal.TokenOps._

case class Context(options: Options)

object ScalaPrinter {

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
    implicit val ctx = Context(options)
    print(root)
  }

  implicit class XtensionDoc(doc: Doc) {
    def wrapped(underlying: Tree): Doc =
      if (needsParens(underlying)) wrapParens(doc)
      else doc
  }

  implicit class XtensionTreeDoc(tree: Tree) {
    def wrapped(implicit ctx: Context): Doc = {
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

    def dFunction(f: Term.Function)(implicit ctx: Context): Doc = {
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
    def unapply(args: List[Tree])(implicit ctx: Context): Option[Doc] =
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
  )(implicit ctx: Context): Doc = {
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
  def dName(name: Tree)(implicit ctx: Context): Doc = name match {
    case _: Name.Anonymous => wildcard
    case _ => print(name)
  }
  def dWithin(keyword: Doc, within: Ref)(implicit ctx: Context): Doc =
    within match {
      case Name.Anonymous() => keyword
      case Term.This(Name.Anonymous()) => keyword + `[` + `this` + `]`
      case _ => dApplyBracket(keyword, within :: Nil)
    }
  def dApplyBrace(fun: Doc, args: List[Tree])(implicit ctx: Context): Doc =
    dApply(fun, args, `{`, `}`)
  def dApplyParen(fun: Doc, args: List[Tree])(implicit ctx: Context): Doc =
    dApply(fun, args, `(`, `)`)
  def dApplyBracket(fun: Doc, args: List[Tree])(implicit ctx: Context): Doc =
    if (args.isEmpty) fun
    else dApply(fun, args, `[`, `]`)
  def dApply(fun: Doc, args: List[Tree], left: Doc, right: Doc)(
      implicit ctx: Context
  ): Doc = {
    val dargs = intercalate(comma + line, args.map(print))
    dargs.tightBracketBy(fun + left, right)
  }
  def dApplyParenPat(fun: Doc, args: List[Pat])(implicit ctx: Context): Doc = {
    val dargs = intercalate(comma + line, args.map(dPat))
    dargs.tightBracketBy(fun + `(`, `)`)
  }

  // This is a quick hack to prevent unnecessary parens.
  def dPath(lhs: Tree, lhsDoc: Doc, sep: Doc, rhs: Doc)(
      implicit ctx: Context
  ): Doc = {
    if (!needsParens(lhs)) lhsDoc + sep + rhs
    else `(` + lhsDoc + `)` + sep + rhs
  }

  def dTyped(lhs: Tree, rhs: Tree)(implicit ctx: Context): Doc = {
    dTyped(lhs, print(rhs))
  }
  def dTyped(lhs: Tree, rhs: Doc)(implicit ctx: Context): Doc = {
    val dlhs = dName(lhs)
    if (needsParens(lhs)) wrapParens(dlhs) + `:` + space + rhs
    else dlhs + typedColon(dlhs) + space + rhs
  }

  def dBlock(stats: List[Tree])(implicit ctx: Context): Doc =
    dBlockI(stats).grouped
  def dBlockI(stats: List[Tree])(implicit ctx: Context): Doc = stats match {
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

  def dMods(mods: List[Mod])(implicit ctx: Context): Doc =
    intercalate(space, mods.map(print))
  def dParamss(paramss: List[List[Term.Param]])(implicit ctx: Context): Doc =
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

  def dBody(body: Tree)(implicit ctx: Context): Doc =
    dBodyO(Some(body))
  def dBodyO(body: Option[Tree])(implicit ctx: Context): Doc =
    body.fold(empty) {
      case t: Term.Block =>
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
  )(implicit ctx: Context): Doc = {
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
  )(implicit ctx: Context): Doc = {
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

  def dStats(stats: List[Tree])(implicit ctx: Context): Doc = {
    intercalate(lineBlank, stats.map {
      case t: Term.Xml => t.wrapped
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

  def dInterpolate(prefix: Name, parts: List[Tree], args: List[Tree])(
      implicit ctx: Context
  ): Doc = {

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
  def dParams(params: List[Term.Param], forceParens: Boolean)(
      implicit ctx: Context
  ): Doc = params match {
    case param :: Nil =>
      val dparam = print(param)
      param.decltpe match {
        case Some(tpe) if forceParens || needsParens(tpe) =>
          `(` + dparam + `)`
        case _ => dparam
      }
    case _ => dApplyParen(empty, params)
  }

  def dPat(pat: Tree)(implicit ctx: Context): Doc =
    print(mkPat(pat))
  def mkPat(pat: Tree)(implicit ctx: Context): Tree =
    pat match {
      case t: Term.Name if t.value.headOption.exists(_.isLower) =>
        PatName(t.value)
      case _ => pat
    }

  private def print(tree: Tree)(implicit ctx: Context): Doc = {
    tree match {
      case t: Name =>
        t match {
          case _: Name.Anonymous => empty
          case _: PatName => backtick + text(t.value) + backtick
          case _ => text(Identifier.backtickWrap(t.value))
        }
      case _: Lit =>
        tree match {
          case t: Lit.Null => `null`
          case t: Lit.Boolean => if (t.value) `true` else `false`
          case t: Lit.String =>
            val (quoteStyle, dquote) = dQuote(t.value)
            val dvalue = dRaw(t.value, quoteStyle)
            dquote + dvalue + dquote
          case _ => text(tree.syntax) // ???
        }
      case _: Enumerator =>
        tree match {
          case t: Enumerator.Generator =>
            dPat(t.pat) + space + `<-` + (line + print(t.rhs)).grouped
          case t: Enumerator.Val =>
            dPat(t.pat) + space + `=` + (line + print(t.rhs)).grouped
          case t: Enumerator.Guard =>
            `if` + space + t.cond.wrapped
        }
      case t: Case =>
        val dbody =
          if (t.body.tokens.isEmpty) empty
          else {
            t.body match {
              case Term.Block(stats) =>
                line + dStats(stats)
              case _ =>
                line + print(t.body)
            }
          }
        `case` + space + dPat(t.pat) +
          t.cond.fold(empty) { c =>
            space + `if` + space + c.wrapped
          } + space + `=>` + dbody.nested(2)
      case _: Type =>
        tree match {
          case t: Type.ByName => `=>` + space + print(t.tpe)
          case t: Type.Select => print(t.qual) + `.` + print(t.name)
          case t: Type.Apply => dApplyBracket(print(t.tpe), t.args)
          case t: Type.ApplyInfix =>
            dInfix(t.lhs, t.op.value, print(t.op), t.rhs :: Nil)
          case t: Type.ImplicitFunction =>
            `implicit` + space + print(Type.Function(t.params, t.res))
          case t: Type.And =>
            print(Type.ApplyInfix(t.lhs, Type.Name("&"), t.rhs))
          case t: Type.Or =>
            print(Type.ApplyInfix(t.lhs, Type.Name("|"), t.rhs))
          case t: Type.With =>
            dInfix(t.lhs, "with", `with`, t.rhs :: Nil)
          case t: Type.Refine =>
            val dtpe = t.tpe.fold(empty) { tpe =>
              val trailingSpace = if (t.stats.nonEmpty) space else empty
              print(tpe) + trailingSpace
            }
            dtpe + dBlock(t.stats)
          case t: Type.Existential =>
            print(t.tpe) + space + `forSome` + space + dBlock(t.stats)
          case t: Type.Annotate =>
            print(t.tpe) + space + spaceSeparated(t.annots.map(print))
          case t: Type.Lambda =>
            dApplyBracket(empty, t.tparams) + space + `=>` + space + print(
              t.tpe
            )
          case t: Type.Placeholder => wildcard + print(t.bounds)
          case t: Type.Repeated => print(t.tpe) + `*`
          case t: Type.Var => print(t.name)
          case t: Type.Function =>
            val dparams = t.params match {
              case Nil => `(` + `)`
              case param :: Nil if !param.is[Type.Tuple] => param.wrapped
              case params => dApplyParen(empty, params)
            }
            dparams + space + `=>` + space + t.res.wrapped
          case t: Type.Tuple => dApplyParen(empty, t.args)
          case t: Type.Project => t.qual.wrapped + `#` + print(t.name)
          case t: Type.Singleton =>
            t.ref match {
              case Term.This(Name.Anonymous()) => `this` + `.` + `type`
              case _ => print(t.ref) + `.` + `type`
            }
        }
      case _: Term =>
        tree match {
          case t: Term.This =>
            t.qual match {
              case _: Name.Anonymous => `this`
              case _ => print(t.qual) + `.` + `this`
            }
          case t: Term.Super =>
            val dthisp = t.thisp match {
              case _: Name.Anonymous => empty
              case thisp => print(thisp) + `.`
            }
            val dsuperp = t.superp match {
              case _: Name.Anonymous => `super`
              case superp => dApplyBracket(`super`, superp :: Nil)
            }
            dthisp + dsuperp
          case t: Term.Select =>
            dPath(t.qual, print(t.qual), `.`, print(t.name))
          case t: Term.Interpolate =>
            dInterpolate(t.prefix, t.parts, t.args)
          case t: Term.Xml =>
            val quoteStyle = TripleQuotes
            val dhead = t.parts.head match {
              case Lit.String(part) => dRaw(part, quoteStyle)
            }
            val dtail = t.parts.tail.zip(t.args).foldLeft(empty) {
              case (accum, (Lit.String(part), arg)) =>
                dApplyBrace(accum, arg :: Nil) + dRaw(part, quoteStyle)
            }
            dhead + dtail
          case t: Term.Return => `return` + space + print(t.expr)
          case t: Term.Repeated => t.expr.wrapped + `:` + wildcard + `*`
          case t: Term.If =>
            def body(expr: Term) = expr match {
              case b: Term.Block => space + dBlock(b.stats)
              case _: Term.If => space + print(expr)
              case _ => (line + print(expr)).nested(2)
            }

            val delse =
              if (t.elsep.tokens.isEmpty) empty
              else
                line + `else` + body(t.elsep)
            (`if` + space + `(` + print(t.cond) + `)` +
              body(t.thenp) +
              delse).grouped
          case t: Term.For =>
            `for` + space + dBlock(t.enums) + space +
              print(t.body)
          case t: Term.ForYield =>
            `for` + space + dBlock(t.enums) + space +
              `yield` + space + print(t.body)
          case t: Term.Block =>
            t.stats match {
              case LambdaArg(doc) => doc
              case _ => dBlock(t.stats)
            }
          case t: Term.PartialFunction =>
            dBlock(t.cases)
          case t: Term.Function =>
            val dbody = (line + print(t.body)).nested(2).grouped
            dParams(t.params, forceParens = true) + space + `=>` + dbody
          case t: Term.Tuple =>
            dApplyParen(empty, t.args)
          case t: Term.Match =>
            t.expr.wrapped + space + `match` + space + dBlock(t.cases)
          case t: Term.Try =>
            val dtry = `try` + space + print(t.expr)
            val dcatch =
              if (t.catchp.isEmpty) empty
              else {
                line + `catch` + space + dBlock(t.catchp)
              }
            val dfinally =
              t.finallyp.fold(empty)(f => line + `finally` + space + print(f))
            (dtry + dcatch + dfinally).grouped
          case t: Term.TryWithHandler =>
            val dtry = `try` + space + print(t.expr) + line +
              `catch` + space + print(t.catchp) +
              t.finallyp.fold(empty)(f => line + `finally` + space + print(f))
            dtry.grouped
          case t: Term.New =>
            `new` + space + print(t.init)
          case t: Term.Assign =>
            print(t.lhs) + space + `=` + space + print(t.rhs)
          case t: Term.Placeholder =>
            `wildcard`
          case t: Term.While =>
            `while` + space + `(` + print(t.expr) + `)` + space + print(t.body)
          case t: Term.Do =>
            `do` + space + print(t.body) + space + `while` + space + `(` + print(t.expr) + `)`
          case t: Term.Throw =>
            `throw` + space + print(t.expr)
          case t: Term.Annotate =>
            dTyped(t.expr, spaceSeparated(t.annots.map(print)))
          case t: Term.NewAnonymous =>
            `new` + print(t.templ)
          case t: Term.Ascribe =>
            dTyped(t.expr, t.tpe)
          case t: Term.Eta =>
            print(t.expr) + space + `wildcard`
          case t: Term.ApplyUnary =>
            val darg =
              if (needsParens(t.arg)) wrapParens(print(t.arg))
              else print(t.arg)
            print(t.op) + darg
          case t: Term.Apply =>
            val dfun = dPath(t.fun, print(t.fun), empty, empty)
            t.args match {
              case LambdaArg(arg) =>
                dfun + space + arg.grouped
              case Term.Block(stats) :: Nil => dfun + space + dBlock(stats)
              case _ => dApplyParen(dfun, t.args)
            }
          case t: Term.ApplyType =>
            dApplyBracket(t.fun.wrapped, t.targs)
          case t: Term.ApplyInfix =>
            val dop = dApplyBracket(print(t.op), t.targs)
            dInfix(t.lhs, t.op.value, dop, t.args)
        }
      case t: Type.Bounds =>
        val dlo = t.lo.fold(empty)(lo => `>:` + space + print(lo))
        val dhi = t.hi.fold(empty) { hi =>
          t.lo.fold(empty)(_ => space) +
            `<:` + space + print(hi)
        }
        val dspace =
          if (t.lo.nonEmpty || t.hi.nonEmpty) space
          else empty
        dspace + dlo + dhi
      case t: Type.Param =>
        val dname = dApplyBracket(dName(t.name), t.tparams)
        val dtbounds = print(t.tbounds)
        val dvbounds = {
          if (t.vbounds.isEmpty) empty
          else {
            space +
              spaceSeparated(t.vbounds.map(tpe => `<%` + space + print(tpe)))
          }
        }
        val dcbounds = t.cbounds match {
          case Nil => empty
          case head :: Nil =>
            `:` + space + print(head)
          case cbounds =>
            space +
              spaceSeparated(cbounds.map(tpe => `:` + space + print(tpe)))
        }
        val dmods =
          if (t.mods.isEmpty) empty
          else dMods(t.mods) + space

        dmods +
          dname +
          dtbounds +
          dvbounds +
          dcbounds
      case t: Term.Param =>
        val ddecltpe =
          t.decltpe.fold(dName(t.name))(tpe => dTyped(t.name, tpe))
        val ddefault =
          t.default.fold(empty)(default => `=` + space + print(default))
        spaceSeparated(
          dMods(t.mods) ::
            ddecltpe ::
            ddefault :: Nil
        )
      case t: Pkg =>
        def guessHasBraces(t: Pkg): Boolean = {
          def isOnlyChildOfOnlyChild(t: Pkg): Boolean = t.parent match {
            case Some(pkg: Pkg) =>
              isOnlyChildOfOnlyChild(pkg) && pkg.stats.length == 1
            case Some(source: Source) => source.stats.length == 1
            case None => true
            case _ => true // ???
          }

          !isOnlyChildOfOnlyChild(t)
        }

        val dstats =
          if (guessHasBraces(t)) space + dBlockI(t.stats)
          else line + dStats(t.stats)
        `package` + space + print(t.ref) + dstats
      case t: Import =>
        `import` + space + intercalate(comma + space, t.importers.map(print))
      case t: Importee =>
        t match {
          case i: Importee.Name => print(i.name)
          case i: Importee.Wildcard => wildcard
          case i: Importee.Rename =>
            print(i.name) + space + `=>` + space + print(i.rename)
          case i: Importee.Unimport =>
            print(i.name) + space + `=>` + space + wildcard
        }
      case t: Importer =>
        val fun = print(t.ref) + `.`
        t.importees match {
          case Importee.Name(n) :: Nil => fun + print(n)
          case Importee.Wildcard() :: Nil => fun + wildcard
          case _ =>
            dApply(fun, t.importees, `{` + space, space + `}`)
        }
      case t: Init =>
        val dfun = t.tpe match {
          case Type.Singleton(Term.This(Name.Anonymous())) => `this`
          case _ => t.tpe.wrapped + print(t.name)
        }
        val dinit = t.argss.foldLeft(dfun) {
          case (accum, args) => dApplyParen(accum, args)
        }
        dinit
      case t: Self =>
        val dname = t.name match {
          case Name.Anonymous() =>
            if (t.decltpe.isDefined) wildcard
            else empty
          case _ =>
            print(t.name)
        }
        val dtpe = t.decltpe.fold(empty)(tpe => `:` + space + print(tpe))
        dname + dtpe + space + `=>`
      case t: Template =>
        val isTermNewAnon = t.parent.exists(_.is[Term.NewAnonymous])
        val dearly = {
          if (t.early.isEmpty) empty
          else if (isTermNewAnon) space + dBlock(t.early)
          else space + `extends` + space + dBlock(t.early)
        }
        val dinits = t.inits match {
          case Nil => empty
          case head :: tail =>
            val keyword =
              if (t.early.isEmpty) {
                if (isTermNewAnon) empty
                else `extends` + space
              } else `with` + space
            val dhead = keyword + print(head)
            val dtail = tail.map(init => `with` + space + print(init))
            (line + intercalate(line, dhead :: dtail)).nested(2).grouped
        }

        val dstats: Doc = {
          val isEmptySelf = isEmpty(t.self)
          if (isEmptySelf &&
            t.stats.isEmpty &&
            !(t.inits.lengthCompare(1) == 0 && isTermNewAnon)) empty
          else {
            val x =
              if (isEmptySelf) dBlock(t.stats)
              else dBlock(t.self :: t.stats)
            space + x
          }
        }

        dearly +
          dinits +
          dstats
      case t: Decl.Def =>
        dDef(
          t.mods,
          `def`,
          print(t.name),
          t.tparams,
          t.paramss,
          Some(t.decltpe)
        )
      case t: Defn.Macro =>
        dDef(
          t.mods,
          `def`,
          print(t.name),
          t.tparams,
          t.paramss,
          t.decltpe,
          `=` + space + `macro` + space + print(t.body)
        )
      case t: Defn.Def =>
        dDef(
          t.mods,
          `def`,
          print(t.name),
          t.tparams,
          t.paramss,
          t.decltpe,
          dBody(t.body)
        )
      case t: Defn.Type =>
        dDef(t.mods, `type`, print(t.name), t.tparams, Nil, None, dBody(t.body))
      case t: Defn.Val =>
        dDefPats(t.mods, `val`, t.pats, Nil, Nil, t.decltpe, dBody(t.rhs))
      case t: Defn.Var =>
        val drhs = Some(t.rhs.fold(Term.Placeholder(): Term)(identity))
        dDefPats(t.mods, `var`, t.pats, Nil, Nil, t.decltpe, dBodyO(drhs))
      case t: Decl.Val =>
        dDefPats(t.mods, `val`, t.pats, Nil, Nil, Some(t.decltpe), empty)
      case t: Decl.Var =>
        dDefPats(t.mods, `var`, t.pats, Nil, Nil, Some(t.decltpe), empty)
      case t: Decl.Type =>
        dDef(t.mods, `type`, print(t.name), t.tparams, Nil, None) +
          print(t.bounds)
      case t: Defn.Class =>
        val dctormods =
          if (t.ctor.mods.isEmpty) empty else space + dMods(t.ctor.mods) + space
        val dsignature =
          dApplyBracket(print(t.name), t.tparams) +
            dctormods +
            dParamss(t.ctor.paramss)
        spaceSeparated(
          dMods(t.mods) ::
            `class` ::
            dsignature ::
            Nil
        ) + print(t.templ)
      case t: Pkg.Object =>
        val ddefn =
          dDef(t.mods, `package` + space + `object`, print(t.name), Nil, Nil)
        ddefn + print(t.templ)
      case t: Defn.Object =>
        val ddefn = dDef(t.mods, `object`, print(t.name), Nil, Nil)
        ddefn + print(t.templ)
      case t: Defn.Trait =>
        val ddefn = dDef(t.mods, `trait`, print(t.name), t.tparams, Nil)
        ddefn + print(t.templ)
      case t: Source =>
        dStats(t.stats)
      case t: Ctor.Secondary =>
        val dbody =
          if (t.stats.isEmpty) dBody(t.init)
          else `=` + space + dBlock(t.init :: t.stats)
        dDef(t.mods, `def`, `this`, Nil, t.paramss, None, dbody)
      case m: Mod =>
        m match {
          case t: Mod.Annot => `@` + print(t.init)
          case _: Mod.Final => `final`
          case _: Mod.Case => `case`
          case _: Mod.Sealed => `sealed`
          case _: Mod.Abstract => `abstract`
          case _: Mod.Implicit => `implicit`
          case _: Mod.Lazy => `lazy`
          case _: Mod.Override => `override`
          case _: Mod.Covariant => covariant
          case _: Mod.Contravariant => contravariant
          case _: Mod.VarParam => `var`
          case _: Mod.ValParam => `val`
          case t: Mod.Private => dWithin(`private`, t.within)
          case t: Mod.Protected => dWithin(`protected`, t.within)
        }
      case p: Pat =>
        p match {
          case t: Pat.Var => print(t.name)
          case t: Pat.Wildcard => wildcard
          case t: Pat.SeqWildcard => wildcard + `*`
          case t: Pat.Bind =>
            val drhs = mkPat(t.rhs).wrapped
            print(t.lhs) + space + `@` + space + drhs
          case t: Pat.Alternative =>
            dPat(t.lhs) + space + `|` + space + dPat(t.rhs)
          case t: Pat.Tuple =>
            dApplyParenPat(empty, t.args)
          case t: Pat.Extract =>
            dApplyParenPat(print(t.fun), t.args)
          case t: Pat.ExtractInfix =>
            dInfix(mkPat(t.lhs), t.op.value, print(t.op), t.rhs.map(mkPat))
          case t: Pat.Interpolate =>
            dInterpolate(t.prefix, t.parts, t.args)
          case t: Pat.Typed =>
            print(t.lhs) + `:` + space + t.rhs.wrapped
        }
    }

  }

}
