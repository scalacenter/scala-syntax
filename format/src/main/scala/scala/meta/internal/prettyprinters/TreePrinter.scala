package scala.meta.internal.prettyprinters

import tokens.SyntaxTokens._

import SyntacticGroup.Pat._
import SyntacticGroup.Term._
import SyntacticGroup.Type._
import CustomTrees.PatName
import ScalaToken._

import scala.meta.internal.paiges.Doc
import scala.meta.internal.paiges.Doc._

import scala.meta.{`package` => _, _}

import scala.language.implicitConversions

object TreePrinter {
  def print(tree: Tree): Doc = {
    val trivia = AssociatedTrivias(tree)
    (new TreePrinter()(trivia)).print(tree)
  }

  def printInput(input: Input, options: Options): Doc = {
    printTree(getRoot(input, options), options)
  }

  def printTree(root: Tree, options: Options): Doc = {
    print(root)
  }

  def getRoot(input: String, options: Options): Tree = {
    getRoot(Input.String(input), options)
  }

  def getRoot(input: Input, options: Options): Tree = {
    options.parser.apply(input, options.dialect).get
  }
}

trait WithPrinter {
  implicit val trivia: AssociatedTrivias
  def print(tree: Tree): Doc
}

class TreePrinter private ()(implicit val trivia: AssociatedTrivias)
    extends WithPrinter
    with TreeDocOps
    with SyntacticGroupOps
    with TreePrinterUtils {

  def print(tree: Tree): Doc = {
    val result = tree match {
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
          case t: Lit.Unit => `(` + `)`
          case t: Lit.Int => text(t.value.toString)
          case t: Lit.Double => text(t.format + "D")
          case t: Lit.Float => text(t.format + "F")
          case t: Lit.Long => text(t.value + "L")
          case t: Lit.Byte => text(t.value + "Z")
          case t: Lit.Short => text(t.value + "S")
          case t: Lit.Char =>
            SingleQuotes + text(
              SyntaxOps.escape(t.value.toString, SingleQuotes)
            ) + SingleQuotes
          case t: Lit.Symbol => SingleQuotes + text(t.value.name)
        }
      case _: Enumerator =>
        tree match {
          case t: Enumerator.Generator =>
            dPat(t.pat) + space + `<-` + (line + print(t.rhs)).grouped
          case t: Enumerator.Val =>
            dPat(t.pat) + space + `=` + (line + print(t.rhs)).grouped
          case t: Enumerator.Guard =>
            `if` + space + PostfixExpr.wrap(t.cond)
        }
      case t: Case =>
        val body =
          if (t.body.tokens.isEmpty) empty
          else {
            t.body match {
              case Term.Block(stats) =>
                line + dStats(stats)
              case _ =>
                line + print(t.body)
            }
          }

        val pat = dPat(t.pat)
        val cond =
          t.cond.fold(empty) { c =>
            space + `if` + space + PostfixExpr.wrap(c)
          }

        `case` + space + pat + cond + space + `=>` + body.nested(2)

      case _: Type =>
        tree match {
          case t: Type.ByName =>
            `=>` + space + print(t.tpe)
          case t: Type.Select =>
            print(t.qual) + `.` + print(t.name)
          case t: Type.Apply =>
            dApplyBracket(print(t.tpe), t.args)
          case t: Type.ApplyInfix =>
            dInfixType(t.lhs, print(t.op), t.rhs)
          case t: Type.ImplicitFunction =>
            `implicit` + space + dTypeFunction(t.params, t.res)
          case t: Type.And =>
            dInfixType(t.lhs, `&`, t.rhs)
          case t: Type.Or =>
            dInfixType(t.lhs, `|`, t.rhs)
          case t: Type.With =>
            dInfixType(t.lhs, `with`, t.rhs)
          case t: Type.Refine =>
            val dtpe = t.tpe.fold(empty) { tpe =>
              val trailingSpace = if (t.stats.nonEmpty) space else empty
              RefineTyp.wrap(tpe) + trailingSpace
            }
            dtpe + dBlock(t.stats)
          case t: Type.Existential =>
            print(t.tpe) + space + `forSome` + space + dBlock(t.stats)
          case t: Type.Annotate =>
            AnnotTyp.wrap(t.tpe) + space + spaceSeparated(t.annots.map(print))
          case t: Type.Lambda =>
            dApplyBracket(empty, t.tparams) + space + `=>` + space + print(
              t.tpe
            )
          case t: Type.Placeholder => wildcard + print(t.bounds)
          case t: Type.Repeated => print(t.tpe) + `*`
          case t: Type.Var => print(t.name)
          case t: Type.Function =>
            dTypeFunction(t.params, t.res)
          case t: Type.Tuple => dApplyParen(empty, t.args)
          case t: Type.Project => SimpleTyp.wrap(t.qual) + `#` + print(t.name)
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
            SimpleExpr.wrap(t.qual) + `.` + print(t.name)
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
          case t: Term.Repeated =>
            PostfixExpr.wrap(t.expr) + `:` + space + wildcard + `*`
          case t: Term.If =>
            def body(expr: Term) = expr match {
              case b: Term.Block => space + dBlock(b.`{`, b.stats, b.`}`)
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
              case _ => dBlock(t.`{`, t.stats, t.`}`)
            }
          case t: Term.PartialFunction =>
            dBlock(`{`, t.cases, `}`)
          case t: Term.Function =>
            val dbody = (line + print(t.body)).nested(2).grouped
            dParams(t.params, forceParens = true) + space + `=>` + dbody
          case t: Term.Tuple =>
            t.args.mkDoc(`(`, List(`,` + space), `)`)

          case t: Term.Match =>
            PostfixExpr.wrap(t.expr) + space + `match` + space + dBlock(t.cases)
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
            val output = `new` + space + print(t.init)

            if (t.init.argss.isEmpty) wrapParens(output)
            else output

          case t: Term.Assign =>
            print(t.lhs) + space + `=` + space + print(t.rhs)
          case t: Term.Placeholder =>
            `wildcard`
          case t: Term.While =>
            `while` + space + `(` + print(t.expr) + `)` + space + print(t.body)
          case t: Term.Do =>
            `do` + space + print(t.body) + space + `while` + space + `(` + print(
              t.expr
            ) + `)`
          case t: Term.Throw =>
            `throw` + space + print(t.expr)
          case t: Term.Annotate =>
            dAscription(t.expr, spaceSeparated(t.annots.map(print)))
          case t: Term.NewAnonymous =>
            `new` + print(t.templ)
          case t: Term.Ascribe =>
            dAscription(t.expr, t.tpe)
          case t: Term.Eta =>
            print(t.expr) + space + `wildcard`
          case t: Term.ApplyUnary =>
            val group = TreeSyntacticGroup(t)
            print(t.op) + group.wrap1(PrefixArg(t.arg), print(t.arg))
          case t: Term.Apply =>
            val dfun = SimpleExpr1.wrap(t.fun)
            t.args match {
              case LambdaArg(arg) =>
                dfun + space + arg.grouped
              case Term.Block(stats) :: Nil =>
                dfun + space + dBlock(stats)
              case _ =>
                dApplyParen(dfun, t.args, t.`,`)
            }
          case t: Term.ApplyType =>
            dApplyBracket(SimpleExpr1.wrap(t.fun), t.targs)
          case t: Term.ApplyInfix =>
            val group = InfixExpr(t.op.value)
            val dlhs = group.wrap(t.lhs, Side.Left)
            val dargs = t.args match {
              case Lit.Unit() :: Nil =>
                `(` + `(` + `)` + `)`
              case (_: Term.Tuple) :: Nil =>
                dArgs(t.args)
              case arg :: Nil =>
                group.wrap(arg, Side.Right)
              case args =>
                dArgs(args)
            }
            val dlhsHasNewline = dlhs.flatten.isEmpty

            val res =
              dlhs + space + print(t.op) + dTargs(t.targs) + space + dargs

            if (dlhsHasNewline) wrapParens(res)
            else res
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
          t.decltpe.fold(dName(t.name))(tpe => dAscription(t.name, tpe))
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
            // $COVERAGE-OFF$
            case _ => true // Impossible ???
            // $COVERAGE-ON$
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
          case (name: Importee.Name) :: Nil => fun + print(name)
          case (wildcard: Importee.Wildcard) :: Nil => fun + print(wildcard)
          case _ => dApply(fun, t.importees, `{` + space, space + `}`)
        }
      case t: Init =>
        val dfun = t.tpe match {
          case Type.Singleton(Term.This(Name.Anonymous())) => `this`
          case _ => AnnotTyp.wrap(t.tpe) + print(t.name)
        }
        val dinit = t.argss.foldLeft(dfun) {
          case (accum, args) => dApplyParen(accum, args)
        }
        dinit
      case t: Self =>
        val dname = t.name match {
          case Name.Anonymous() =>
            if (t.decltpe.isDefined) wildcard
            // $COVERAGE-OFF$
            // since `trait A { this => }` is simply `trait A { }`
            else empty
          // $COVERAGE-ON$
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
          case Nil if isTermNewAnon => space + `{` + `}`
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
          t.`,`,
          Some(t.decltpe)
        )
      case t: Defn.Macro =>
        dDef(
          t.mods,
          `def`,
          print(t.name),
          t.tparams,
          t.paramss,
          t.`,`,
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
          t.`,`,
          t.decltpe,
          dBody(t.body)
        )
      case t: Defn.Type =>
        dDef(
          t.mods,
          `type`,
          print(t.name),
          t.tparams,
          Nil,
          Nil,
          None,
          dBody(t.body)
        )
      case t: Defn.Val =>
        dDefPats(t.mods, `val`, t.pats, Nil, Nil, Nil, t.decltpe, dBody(t.rhs))
      case t: Defn.Var =>
        val drhs = Some(t.rhs.fold(Term.Placeholder(): Term)(identity))
        dDefPats(t.mods, `var`, t.pats, Nil, Nil, Nil, t.decltpe, dBodyO(drhs))
      case t: Decl.Val =>
        dDefPats(t.mods, `val`, t.pats, Nil, Nil, Nil, Some(t.decltpe), empty)
      case t: Decl.Var =>
        dDefPats(t.mods, `var`, t.pats, Nil, Nil, Nil, Some(t.decltpe), empty)
      case t: Decl.Type =>
        dDef(t.mods, `type`, print(t.name), t.tparams, Nil, Nil, None) +
          print(t.bounds)
      case t: Defn.Class =>
        val dctormods =
          if (t.ctor.mods.isEmpty) empty else space + dMods(t.ctor.mods) + space
        val dsignature =
          dApplyBracket(print(t.name), t.tparams) +
            dctormods +
            dParamss(t.ctor.paramss, t.ctor.`,`)
        spaceSeparated(
          dMods(t.mods) ::
            `class` ::
            dsignature ::
            Nil
        ) + print(t.templ)
      case t: Pkg.Object =>
        val ddefn =
          dDef(
            t.mods,
            `package` + space + `object`,
            print(t.name),
            Nil,
            Nil,
            Nil
          )
        ddefn + print(t.templ)
      case t: Defn.Object =>
        val ddefn = dDef(t.mods, `object`, print(t.name), Nil, Nil, Nil)
        ddefn + print(t.templ)
      case t: Defn.Trait =>
        val ddefn = dDef(t.mods, `trait`, print(t.name), t.tparams, Nil, Nil)
        ddefn + print(t.templ)
      case t: Source =>
        dStats(t.stats)
      case t: Ctor.Secondary =>
        val dbody =
          if (t.stats.isEmpty) dBody(t.init)
          else `=` + space + dBlock(t.init :: t.stats)
        dDef(t.mods, `def`, `this`, Nil, t.paramss, t.`,`, None, dbody)
      case m: Mod =>
        m match {
          case t: Mod.Annot =>
            `@` + SimpleTyp.wrap(t.init.tpe) + dArgss(t.init.argss)
          case t: Mod.Private => dWithin(`private`, t.within)
          case t: Mod.Protected => dWithin(`protected`, t.within)
          case _ => {
            m match {
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
              case _: Mod.Inline => `inline`
            }
          }
        }
      case p: Pat =>
        p match {
          case t: Pat.Var => print(t.name)
          case t: Pat.Wildcard => wildcard
          case t: Pat.SeqWildcard => wildcard + `*`
          case t: Pat.Bind =>
            SimplePattern.wrap(t.lhs) + space + `@` + space +
              AnyPattern3.wrap(mkPat(t.rhs))
          case t: Pat.Alternative =>
            dPat(t.lhs) + space + `|` + space + dPat(t.rhs)
          case t: Pat.Tuple =>
            dApplyParenPat(empty, t.args)
          case t: Pat.Extract =>
            dApplyParenPat(print(t.fun), t.args)
          case t: Pat.ExtractInfix =>
            val operator = t.op.value
            val right =
              t.rhs.map(mkPat) match {
                case single :: Nil => {
                  Pattern3(operator).wrap(single, Side.Right)
                }
                case multiple => {
                  dApplyParen(empty, multiple)
                }
              }

            Pattern3(operator).wrap(mkPat(t.lhs)) +
              space +
              print(t.op) +
              space +
              right

          case t: Pat.Interpolate =>
            dInterpolate(t.prefix, t.parts, t.args)
          case t: Pat.Typed =>
            print(t.lhs) + `:` + space + RefineTyp.wrap(t.rhs) // TypedPat.wrap
          case t: Pat.Xml =>
            dPatXml(t)
        }
    }
    Comments(tree, result)
  }

  private implicit def toDoc(quote: QuoteStyle): Doc = text(quote.toString)
}
