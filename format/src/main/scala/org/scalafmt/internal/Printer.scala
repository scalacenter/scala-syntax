package org.scalafmt.internal

import org.scalafmt.Options
import scala.meta.Decl
import scala.meta.Defn
import scala.meta.Token
import scala.meta.Importee
import scala.meta.Importer
import scala.meta.Init
import scala.meta.Lit
import scala.meta.Name
import scala.meta.Self
import scala.meta.Source
import scala.meta.Stat
import scala.meta.Template
import scala.meta.Type
import scala.meta.Term
import scala.meta.Tree
import org.scalameta.logger

class Printer(code: String, options: Options) {

  def parsed: Tree = {
    import scala.meta._
    code.parse[Source].get
  }
  import org.typelevel.paiges.Doc
  import org.typelevel.paiges.Doc._
  val dot = char('.')
  val leftParen = char('(')
  val rightParen = char(')')
  val leftBracket = char('[')
  val rightBracket = char(']')
  val leftBrace = char('{')
  val rightBrace = char('}')
  val colon = char(':')
  val `null` = text("null")
  val `true` = text("true")
  val `false` = text("false")
  val `object` = text("object")
  val `trait` = text("trait")
  val `class` = text("class")
  val `case` = text("case")
  val `if` = text("if")
  val `else` = text("else")
  val `with` = text("with")
  val `package` = text("package")
  val `match` = text("match")
  val `extends` = text("extends")
  val blankLine = line + line

  def docApplyParen(fun: Doc, args: List[Tree]): Doc =
    docApply(fun, args, leftParen, rightParen)
  def docApplyBracket(fun: Doc, args: List[Tree]): Doc =
    docApply(fun, args, leftBracket, rightBracket)
  def docApply(fun: Doc, args: List[Tree], left: Doc, right: Doc): Doc = {
    val dargs = intercalate(comma + line, args.map(print))
    logger.elem(dargs.render(80))
    dargs.tightBracketBy(fun + left, right)
  }

  def docBlock(stats: List[Tree]): Doc = stats match {
    case Nil => empty
    case _ =>
      intercalate(blankLine, stats.map(print))
        .tightBracketBy(leftBrace, rightBrace)
  }

  def isEmpty(self: Self): Boolean = self match {
    case Self(Name.Anonymous(), None) => true
    case _ => false
  }

  def print(tree: Tree): Doc = tree match {
    case Name.Anonymous() => empty
    case Term.Name(value) => text(value)
    case Term.Name(value) => text(value)
    case Type.Name(value) => text(value)
    case Term.Select(qual, name) => print(qual) + dot + print(name)
    case Type.Select(qual, name) => print(qual) + dot + print(name)
    case Term.Apply(fun, args) => docApplyParen(print(fun), args)
    case Term.ApplyType(fun, targs) => docApplyBracket(print(fun), targs)
    case Term.ApplyInfix(lhs, op, targs, args) =>
      import scala.meta._
      if (targs.isEmpty) print(q"$lhs.$op(..$args)")
      else print(q"$lhs.$op[..$targs](..$args)")
    case Type.Apply(fun, args) => docApplyBracket(print(fun), args)
    case Lit.Null() => `null`
    case Lit.Boolean(value) => if (value) `true` else `false`
    case Lit.Char(value) => char(value)
    case _: Lit => text(tree.syntax) // ???
    case Init(tpe, name, argss) =>
      val dfun = print(tpe) + print(name)
      val dinit = argss.foldLeft(dfun) {
        case (accum, args) => docApplyParen(accum, args)
      }
      dinit
    case Self(name, decltpe) =>
      val dname = name match {
        case Name.Anonymous() => empty
        case _ => print(name)
      }
      val dtpe = decltpe.fold(empty)(tpe => colon + space + print(tpe))
      dname + dtpe
    case Template(early, inits, self, stats) =>
      val dearly = docBlock(early)
      pprint.log(dearly.render(80))
      val dinits = inits match {
        case Nil => empty
        case head :: tail =>
          val dhead = `extends` + space + print(head)
          val dtail = tail.map(init => `with` + space + print(init))
          intercalate(line, dhead :: dtail).tightBracketBy(empty, empty)
      }
      val dstats =
        if (isEmpty(self)) docBlock(stats)
        else docBlock(self :: stats)
      intercalate(
        space,
        (dearly :: dinits :: dstats :: Nil).filterNot(_.isEmpty)
      )
    case Defn.Object(mods, name, templ) =>
      val dmods = intercalate(space, mods.map(print))
      intercalate(
        space,
        dmods :: `object` :: print(name) :: print(templ) :: Nil
      )
    case Source(stats) => intercalate(blankLine, stats.map(print))
  }
}
