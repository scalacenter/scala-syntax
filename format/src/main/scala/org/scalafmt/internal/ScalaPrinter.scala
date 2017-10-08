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
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc._
import ScalaToken._
import org.langmeta.inputs.Input
import org.scalameta.logger
import org.scalameta.logger.revealWhitespace

class ScalaPrinter(code: Input, options: Options) {

  lazy val root: Tree = {
    options.parser.apply(code, options.dialect).get
  }
//  pprint.log(root)

  def dName(name: Name): Doc = name match {
    case _: Name.Anonymous => wildcard
    case _ => print(name)
  }
  def dWithin(keyword: Doc, within: Ref): Doc = within match {
    case Name.Anonymous() => keyword
    case Term.This(Name.Anonymous()) => keyword + `[` + `this` + `]`
    case _ => dApplyBracket(keyword, within :: Nil)
  }
  def dApplyBrace(fun: Doc, args: List[Tree]): Doc =
    dApply(fun, args, `{`, `}`)
  def dApplyParen(fun: Doc, args: List[Tree]): Doc =
    dApply(fun, args, `(`, `)`)
  def dApplyBracket(fun: Doc, args: List[Tree]): Doc =
    if (args.isEmpty) fun
    else dApply(fun, args, `[`, `]`)
  def dApply(fun: Doc, args: List[Tree], left: Doc, right: Doc): Doc = {
    val dargs = intercalate(comma + line, args.map(print))
    dargs.tightBracketBy(fun + left, right)
  }

  def dTyped(lhs: Tree, rhs: Tree) = {
    val dlhs = print(lhs)
    dlhs + typedColon(dlhs) + space + print(rhs)
  }

  def dBlock(stats: List[Tree]): Doc = stats match {
    case Nil => empty
    case head :: Nil =>
      `{` + ((line + print(head)).nested(2) + line) + `}`
    case _ =>
      `{` +
        (line + intercalate(lineBlank, stats.map(print))).nested(2) +
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
    if (TokenOps.needsLeadingSpaceBeforeColon(lhs.render(100))) space + `:`
    else `:`

  def dmods(mods: List[Mod]): Doc = intercalate(space, mods.map(print))
  def ddef(
      mods: List[Mod],
      keyword: Doc,
      pats: List[Pat],
      tparams: List[Type.Param],
      paramss: List[List[Term.Param]],
      decltpe: Option[Type],
      body: Option[Tree]
  ): Doc = {
    val dname = commaSeparated(pats.map(print))
    ddef(mods, keyword, dname, tparams, paramss, decltpe, body)
  }
  def dParamss(paramss: List[List[Term.Param]]): Doc =
    joined(paramss.map(params => dApplyParen(empty, params)))
  def ddef(
      mods: List[Mod],
      keyword: Doc,
      name: Doc,
      tparams: List[Type.Param],
      paramss: List[List[Term.Param]],
      decltpe: Option[Type] = None,
      body: Option[Tree] = None
  ): Doc = {
    val dname = dApplyBracket(name, tparams)
    val dparamss = dParamss(paramss)
    val ddecltpe =
      decltpe.fold(empty)(tpe => typedColon(name) + space + print(tpe))
    val dbody = body.fold(empty) {
      case t: Term.Block =>
        `=` + space + print(t)
      case t =>
        `=` + (line + print(t)).nested(2).grouped
    }

    spaceSeparated(
      dmods(mods) ::
        keyword ::
        dname + dparamss + ddecltpe ::
        dbody ::
        Nil
    )
  }

  def dStats(stats: List[Stat]): Doc =
    intercalate(lineBlank, stats.map(print))

  def dRaw(str: String, start: Int = 0): Doc = {
    if (start >= str.length) empty
    else {
      val idx = str.indexOf('\n', start)
      if (idx < 0) text(str.substring(start))
      else {
        text(str.substring(start, idx)) +
          lineNoFlatNoIndent +
          dRaw(str, idx + 1)
      }
    }
  }

  def isMultiline(part: String): Boolean =
    part.contains("\n") ||
      (part.contains("\"") && !part.contains("\"\"\""))

  def dQuote(str: String): Doc = if (isMultiline(str)) `"""` else `"`

  def dInterpolate(prefix: Name, parts: List[Tree], args: List[Tree]): Doc = {
    val isTripeQuoted = parts.exists {
      case Lit.String(part) => isMultiline(part)
    }
    val dquote = if (isTripeQuoted) `"""` else `"`
    val dhead = parts.head match { case l @ Lit.String(value) => dRaw(value) }
    val sparts = parts.tail.zip(args).foldLeft(empty) {
      case (accum, (Lit.String(part), name: Term.Name))
          if !TokenOps.isIdentifierStart(part) =>
        accum + `$` + print(name) + dRaw(part)
      case (accum, (Lit.String(part), arg)) =>
        accum + dApplyBrace(`$`, arg :: Nil) + dRaw(part)
    }
    print(prefix) + dquote + dhead + sparts + dquote
  }
  def dParams(params: List[Term.Param]) = params match {
    case Nil => ???
    case param :: Nil => print(param)
    case _ => dApplyParen(empty, params)
  }

  def print(tree: Tree): Doc = tree match {
    case t: Name =>
      t match {
        case _: Name.Anonymous => empty
        case _ => text(t.value)
      }
    case _: Lit =>
      tree match {
        case t: Lit.Null => `null`
        case t: Lit.Boolean => if (t.value) `true` else `false`
        case t: Lit.Char => char(t.value)
        case t: Lit.String =>
          val dquote = dQuote(t.value)
          dquote + dRaw(t.value) + dquote
        case _ => text(tree.syntax) // ???
      }
    case _: Enumerator =>
      tree match {
        case t: Enumerator.Generator =>
          print(t.pat) + space + `<-` + (line + print(t.rhs)).grouped
        case t: Enumerator.Val =>
          print(t.pat) + space + `=` + (line + print(t.rhs)).grouped
        case t: Enumerator.Guard =>
          `if` + space + print(t.cond)
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
      `case` + space + print(t.pat) +
        t.cond.fold(empty) { c =>
          space + `if` + space +
            print(c)
        } + space + `=>` + dbody.nested(2)
    case _: Type =>
      tree match {
        case t: Type.ByName => `=>` + space + print(t.tpe)
        case t: Type.Select => print(t.qual) + `.` + print(t.name)
        case t: Type.Apply => dApplyBracket(print(t.tpe), t.args)
        case t: Type.ApplyInfix =>
          `(` + print(t.lhs) + space + print(t.op) + space + print(t.rhs) + `)`
        case t: Type.ImplicitFunction =>
          `implicit` + space + print(Type.Function(t.params, t.res))
        case t: Type.And => print(Type.ApplyInfix(t.lhs, Type.Name("&"), t.rhs))
        case t: Type.Or => print(Type.ApplyInfix(t.lhs, Type.Name("|"), t.rhs))
        case t: Type.With =>
          print(Type.ApplyInfix(t.lhs, Type.Name("with"), t.rhs))
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
          dApplyBracket(empty, t.tparams) + space + `=>` + space + print(t.tpe)
        case t: Type.Placeholder => wildcard + print(t.bounds)
        case t: Type.Repeated => print(t.tpe) + `*`
        case t: Type.Var => print(t.name)
        case t: Type.Function =>
          val dparams = t.params match {
            case Nil => `(` + `)`
            case param :: Nil => print(param)
            case params => dApplyParen(empty, params)
          }
          dparams + space + `=>` + space + print(t.res)
        case t: Type.Tuple => dApplyParen(empty, t.args)
        case t: Type.Project => print(t.qual) + `#` + print(t.name)
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
        case t: Term.Select => print(t.qual) + `.` + print(t.name)
        case t: Term.Interpolate =>
          dInterpolate(t.prefix, t.parts, t.args)
        case t: Term.Xml =>
          val dhead = t.parts.head match { case Lit.String(part) => text(part) }
          val dtail = t.parts.tail.zip(t.args).foldLeft(empty) {
            case (accum, (Lit.String(part), arg)) =>
              dApplyBrace(accum, arg :: Nil) + text(part)
          }
          dhead + dtail
        case t: Term.Return => `return` + space + print(t.expr)
        case t: Term.Repeated => print(t.expr) + `:` + wildcard + `*`
        case t: Term.If =>
          def body(expr: Term) = expr match {
            case b: Term.Block => space + dBlock(b.stats)
            case _ => (line + print(expr)).nested(2)
          }
          val delse =
            if (t.elsep.tokens.isEmpty) empty
            else
              line + `else` + body(t.elsep)
          `if` + space + `(` + print(t.cond) + `)` +
            body(t.thenp) +
            delse
        case t: Term.For =>
          `for` + space + dBlock(t.enums) + space +
            print(t.body)
        case t: Term.ForYield =>
          `for` + space + dBlock(t.enums) + space +
            `yield` + space + print(t.body)
        case t: Term.Block => dBlock(t.stats)
        case t: Term.PartialFunction =>
          dBlock(t.cases)
        case t: Term.Function =>
          val dbody = (line + print(t.body)).nested(2).grouped
          dParams(t.params) + space + `=>` + dbody
        case t: Term.Tuple =>
          dApplyParen(empty, t.args)
        case t: Term.Match =>
          print(t.expr) + space + `match` + space + dBlock(t.cases)
        case t: Term.Try =>
          val dtry = `try` + space + print(t.expr) + line +
            `catch` + space + dBlock(t.catchp) +
            t.finallyp.fold(empty)(f => line + `finally` + space + print(f))
          dtry.grouped
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
          `do` + print(t.body) + space + `while` + space + `(` + print(t.expr) + `)`
        case t: Term.Throw =>
          `throw` + space + print(t.expr)
        case t: Term.Annotate =>
          `(` + spaceSeparated(print(t.expr) :: t.annots.map(print)) + `)`
        case t: Term.NewAnonymous =>
          `new` + space + print(t.templ)
        case t: Term.Ascribe =>
          `(` + dTyped(t.expr, t.tpe) + `)`
        case t: Term.Eta =>
          print(t.expr) + space + `wildcard`
        case t: Term.ApplyUnary =>
          print(t.op) + print(t.arg)
        case t: Term.Apply =>
          t.args match {
            case (arg: Term.PartialFunction) :: Nil =>
              print(t.fun) + space + print(arg)
            case (arg @ Term.Block((f: Term.Function) :: Nil)) :: Nil =>
              val dbody = f.body match {
                case Term.Block(stats) => dStats(stats)
                case _ => print(f.body)
              }
              print(t.fun) + space + `{` +
                ((line + dParams(f.params) + space + `=>`)
                  .nested(2)
                  .grouped + line + dbody)
                  .nested(2)
                  .grouped +
                line + `}`
            case _ =>
              dApplyParen(print(t.fun), t.args)
          }
        case t: Term.ApplyType => dApplyBracket(print(t.fun), t.targs)
        case t: Term.ApplyInfix =>
          `(` +
            print(t.lhs) + space +
            print(t.op) + space +
            dApplyParen(empty, t.args) +
            `)`
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
      dmods(t.mods) +
        dname +
        dtbounds +
        dvbounds +
        dcbounds
    case t: Term.Param =>
      val dname =
        t.decltpe.fold(print(t.name))(tpe => dTyped(t.name, tpe))
      val ddefault =
        t.default.fold(empty)(default => space + `=` + print(default))
      spaceSeparated(
        dmods(t.mods) ::
          dname ::
          ddefault ::
          Nil
      )
    case t: Pkg =>
      `package` + space + print(t.ref) + line + dStats(t.stats)
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
        case _ => print(t.tpe) + print(t.name)
      }
      val dinit = t.argss.foldLeft(dfun) {
        case (accum, args) => dApplyParen(accum, args)
      }
      dinit
    case t: Self =>
      val dname = t.name match {
        case Name.Anonymous() => empty
        case _ => print(t.name)
      }
      val dtpe = t.decltpe.fold(empty)(tpe => `:` + space + print(tpe))
      dname + dtpe + space + `=>`
    case t: Template =>
      val dearly = {
        if (t.early.isEmpty) empty
        else space + `extends` + space + dBlock(t.early)
      }
      val dinits = t.inits match {
        case Nil => empty
        case head :: tail =>
          val keyword = if (t.early.isEmpty) `extends` else `with`
          val dhead = keyword + space + print(head)
          val dtail = tail.map(init => `with` + space + print(init))
          (line + intercalate(line, dhead :: dtail)).nested(2).grouped
      }
      val dstats = {
        val dblock =
          if (isEmpty(t.self)) dBlock(t.stats)
          else dBlock(t.self :: t.stats)
        space + dblock
      }

      dearly +
        dinits +
        dstats
    case t: Decl.Def =>
      ddef(
        t.mods,
        `def`,
        print(t.name),
        t.tparams,
        t.paramss,
        Some(t.decltpe)
      )
    case t: Defn.Macro =>
      ddef(
        t.mods,
        `def`,
        print(t.name),
        t.tparams,
        t.paramss,
        t.decltpe,
        Some(t.body)
      )
    case t: Defn.Def =>
      ddef(
        t.mods,
        `def`,
        print(t.name),
        t.tparams,
        t.paramss,
        t.decltpe,
        Some(t.body)
      )
    case t: Defn.Type =>
      ddef(t.mods, `type`, print(t.name), t.tparams, Nil, None, Some(t.body))
    case t: Defn.Val =>
      ddef(t.mods, `val`, t.pats, Nil, Nil, t.decltpe, Some(t.rhs))
    case t: Defn.Var =>
      val drhs = Some(t.rhs.fold(Term.Name("_"): Term)(identity))
      ddef(t.mods, `var`, t.pats, Nil, Nil, t.decltpe, drhs)
    case t: Decl.Val =>
      ddef(t.mods, `val`, t.pats, Nil, Nil, Some(t.decltpe), None)
    case t: Decl.Var =>
      ddef(t.mods, `var`, t.pats, Nil, Nil, Some(t.decltpe), None)
    case t: Decl.Type =>
      ddef(t.mods, `type`, print(t.name), t.tparams, Nil, None, None)
    case t: Defn.Class =>
      val dctormods =
        if (t.ctor.mods.isEmpty) empty else space + dmods(t.ctor.mods) + space
      val dsignature =
        dApplyBracket(print(t.name), t.tparams) +
          dctormods +
          dParamss(t.ctor.paramss)
      spaceSeparated(
        dmods(t.mods) ::
          `class` ::
          dsignature ::
          Nil
      ) + print(t.templ)
    case t: Pkg.Object =>
      val ddefn =
        ddef(t.mods, `package` + space + `object`, print(t.name), Nil, Nil)
      ddefn + print(t.templ)
    case t: Defn.Object =>
      val ddefn = ddef(t.mods, `object`, print(t.name), Nil, Nil)
      ddefn + print(t.templ)
    case t: Defn.Trait =>
      val ddefn = ddef(t.mods, `trait`, print(t.name), t.tparams, Nil)
      ddefn + print(t.templ)
    case t: Source =>
      dStats(t.stats)
    case t: Ctor.Secondary =>
      ddef(t.mods, `def`, `this`, Nil, t.paramss, None, Some(t.init))
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
          print(t.lhs) + space + `@` + space + print(t.rhs)
        case t: Pat.Alternative =>
          print(t.lhs) + space + `|` + space + print(t.rhs)
        case t: Pat.Tuple =>
          dApplyParen(empty, t.args)
        case t: Pat.ExtractInfix =>
          val drhs = t.rhs match {
            case Nil => ???
            case rhs :: Nil => print(rhs)
            case _ => dApplyParen(empty, t.rhs)
          }
          print(t.lhs) + space + print(t.op) + space + drhs
        case t: Pat.Interpolate =>
          dInterpolate(t.prefix, t.parts, t.args)
        case t: Pat.Typed =>
          print(t.lhs) + `:` + space + print(t.rhs)
        case t: Pat.Extract =>
          dApplyParen(print(t.fun), t.args)
      }
  }
}
