package org.scalafmt.internal

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

class ScalaPrinter(code: Input, options: Options) {

  def parsed: Tree = {
    options.parser.apply(code, options.dialect).get
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
      `{` + ((line + print(head)).nested(2) + line).grouped + `}`
    case _ =>
      `{` +
        (lineBreak + intercalate(lineBreak, stats.map(print))).nested(2) +
        lineBreak + `}`
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
    val dbody =
      body.fold(empty)(t => `=` + (line + print(t)).nested(2).grouped)
    spaceSeparated(
      dmods(mods) ::
        keyword ::
        dname + dparamss + ddecltpe ::
        dbody ::
        Nil
    )
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
      `case` + space + print(t.pat) +
        t.cond.fold(empty)(c => space + `if` + print(c)) + space +
        `=>`
    case _: Type =>
      tree match {
        case t: Type.Select => print(t.qual) + `.` + print(t.name)
        case t: Type.Apply => dApplyBracket(print(t.tpe), t.args)
      }
    case _: Term =>
      tree match {
        case t: Term.Return => `return` + space + print(t.expr)
        case t: Term.Repeated => print(t.expr) + `:` + wilcard + `*`
        case t: Term.If =>
          val delse =
            if (t.elsep.tokens.isEmpty) empty
            else
              (
                lineBreak + `else` + (line + print(t.elsep)).nested(2)
              ).grouped
          pprint.log(delse.render(100))
          `if` + space + `(` + print(t.cond) + `)` +
            (line + print(t.thenp)).nested(2).grouped +
            delse
        case t: Term.For =>
          `for` + space + dBlock(t.enums) + space +
            print(t.body)
        case t: Term.ForYield =>
          `for` + space + dBlock(t.enums) + space +
            `yield` + space + print(t.body)
        case t: Term.Block => dBlock(t.stats)
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
        case t: Term.Select => print(t.qual) + `.` + print(t.name)
        case t: Term.Apply => dApplyParen(print(t.fun), t.args)
        case t: Term.ApplyType => dApplyBracket(print(t.fun), t.targs)
        case t: Term.ApplyInfix =>
          import scala.meta._
          import t._
          if (targs.isEmpty) print(q"$lhs.$op(..$args)")
          else print(q"$lhs.$op[..$targs](..$args)")
      }
    case t: Type.Param =>
      spaceSeparated(
        dmods(t.mods) ::
          print(t.name) ::
          Nil // ???
      )
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
      `package` + space + intercalate(
        line,
        print(t.ref) :: t.stats.map(print)
      )
    case t: Import =>
      `import` + space + intercalate(comma + space, t.importers.map(print))
    case t: Importee =>
      t match {
        case i: Importee.Name => print(i.name)
        case i: Importee.Wildcard => wilcard
        case i: Importee.Rename =>
          print(i.name) + space + `=>` + space + print(i.rename)
        case i: Importee.Unimport =>
          print(i.name) + space + `=>` + space + wilcard
      }
    case t: Importer =>
      val fun = print(t.ref) + `.`
      t.importees match {
        case Importee.Name(n) :: Nil => fun + print(n)
        case Importee.Wildcard() :: Nil => fun + wilcard
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
      pprint.log(dearly.render(20))
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
        dblock
      }
      intercalate(
        empty,
        (dearly :: dinits :: dstats :: Nil).filterNot(_.isEmpty)
      )
    case t: Decl.Def =>
      ddef(t.mods, `def`, print(t.name), t.tparams, t.paramss, Some(t.decltpe))
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
    case t: Defn.Val =>
      ddef(t.mods, `val`, t.pats, Nil, Nil, t.decltpe, Some(t.rhs))
    case t: Defn.Var =>
      val drhs = Some(t.rhs.fold(Term.Name("_"): Term)(identity))
      ddef(t.mods, `var`, t.pats, Nil, Nil, t.decltpe, drhs)
    case t: Decl.Val =>
      ddef(t.mods, `val`, t.pats, Nil, Nil, Some(t.decltpe), None)
    case t: Decl.Var =>
      ddef(t.mods, `var`, t.pats, Nil, Nil, Some(t.decltpe), None)
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
    case t: Defn.Object =>
      val ddefn = ddef(t.mods, `object`, print(t.name), Nil, Nil)
      ddefn + print(t.templ)
    case t: Defn.Trait =>
      val ddefn = ddef(t.mods, `object`, print(t.name), t.tparams, Nil)
      ddefn + print(t.templ)
    case t: Source =>
      intercalate(lineBlank, t.stats.map(print))
    case t: Ctor.Secondary =>
      ddef(t.mods, `def`, `this`, Nil, t.paramss, None, Some(t.init))
    case m: Mod =>
      m match {
        case _: Mod.Final => `final`
        case _: Mod.Case => `case`
        case t: Mod.Private => dWithin(`private`, t.within)
        case t: Mod.Protected => dWithin(`protected`, t.within)
      }
    case p: Pat =>
      p match {
        case t: Pat.Var => print(t.name)
        case t: Pat.Wildcard => wilcard
        case t: Pat.Typed => `(` + dTyped(t.lhs, t.rhs) + `)`
        case t: Pat.Bind =>
          print(t.lhs) + space + `@` + space + print(t.rhs)
        case t: Pat.Extract =>
          dApplyParen(print(t.fun), t.args)
      }
  }

  def dWithin(keyword: Doc, within: Ref): Doc = within match {
    case Name.Anonymous() => keyword
    case _ => dApplyBracket(keyword, within :: Nil)
  }
}
