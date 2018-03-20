// format: off
package org.scalafmt.tests

import scala.meta.{Pkg, Term, Lit, Input}

import java.nio.charset.StandardCharsets

object CoverageSuite extends BaseScalaPrinterTest {
  val dq = '"'
  val tq = s"${dq}${dq}${dq}"
  val escapeChars = List('b', 't', 'n', 'f', 'r', '"', '\'', '\\')
  val allEscapeChars = escapeChars.mkString(" \\", " \\", "")
  
  def resource(path: String): Input = 
    Input.Stream(
      this.getClass.getClassLoader.getResourceAsStream(path),
      StandardCharsets.UTF_8
    )


  // meta.Lit
  check("null")                    // Lit.Null
  check("true")                    // Lit.Boolean
  check("false")                   // Lit.Boolean
  check("()")                      // Lit.Unit
  check("1")                       // Lit.Int
  checkStructural("-0D")           // Lit.Double scalameta#1385 (-0 not preserved)
  check("0D")                      // Lit.Double
  checkStructural("1.0")           // Lit.Double
  checkStructural("1.0d")          // Lit.Double
  check("4F")                      // Lit.Float
  check("1.0F")                    // Lit.Float
  checkStructural("1.0f")          // Lit.Float
  check("1L")                      // Lit.Long
  checkStructural("1l")            // Lit.Long
  // check("1Z")                   // Lit.Byte  scalameta#1324 (cannot parse Z and S)
  check(Lit.Byte(2), "2Z")         // Lit.Byte
  // check("1S")                   // Lit.Short scalameta#1324
  check(Lit.Short(2), "2S")        // Lit.Short
  check("'a'")                     // Lit.Char
  check("'\\n'")                   // Lit.Char
  check("'a")                      // Lit.Symbol
  check(s"${dq}hello${dq}")        // Lit.String
  check(s"${tq}hello\nworld${tq}") // Lit.String

  // tests/slow/target/repos/scala-js/test-suite/js/src/test/resources/SourceMapTestTemplate.scala
  // check(resource("unicode.scala")) // Lit.String (todo)
  // Lit.String (todo)
  // check(s"""
  //   val b = ${tq}
  //              \\u005Cu0041${tq}
  // """)

  checkStructural(s"${dq}${allEscapeChars}${dq}") // Lit.String scalameta#1384 (\n not preserved)
  

  // meta.Enumerator
  checkEnumerator("`a` <- b") // Enumerator.Generator
  checkEnumerator("a <- b")   // Enumerator.Generator
  checkEnumerator("a = 1")    // Enumerator.Val
  checkEnumerator("if x")     // Enumerator.Guard

  // meta.Case
  checkCase("case `a` =>")
  checkCase("case `a` :: `b` :: _ =>")
  checkCase(""" case a b `c` =>""")
  checkCase("case _ op (a | b) =>")
  checkCase("""case x `.y` () =>""")
  checkCase("case a if p =>")
  checkCaseStructural("case _ => ()")
  checkCaseStructural("case _ => {}")

  // meta.Type
  checkType("B")                    // Type.Name
  checkType("a.B")                  // Type.Select
  checkType("a#B")                  // Type.Project
  checkType("this.type")            // Type.Singleton
  checkType("t.type")               // Type.Singleton
  checkType("F[T]")                 // Type.Apply
  checkType("K Map V")              // Type.ApplyInfix
  checkType("() => B")              // Type.Function
  checkType("A => B")               // Type.Function
  checkType("(A, B) => C")          // Type.Function
  checkType("implicit A => B")      // Type.ImplicitFunction
  checkType("(A, B)")               // Type.Tuple
  checkType("A with B")             // Type.With
  checkType("A & B")                // Type.And
  checkType("A | B", dotty)         // Type.Or
  checkType("A { def f: B }")       // Type.Refine
  checkType("A{}")                  // Type.Refine
  checkType("{ def f: B }")         // Type.Refine
  checkType("A forSome { type T }") // Type.Existential
  checkType("T @A")                 // Type.Annotate
  checkType("[X] => (X, X)")        // Type.Lambda
  checkType("_")                    // Type.Placeholder
  checkType("_ >: A <: B")          // Type.Bounds
  checkType("_ <: B")               // Type.Bounds (lower)
  checkType("_ >: A")               // Type.Bounds (upper)
  check("def f[A <% B[A]]: C")      // Type.Bounds (view)
  check("def f[A: B]: C")           // Type.Bounds (context)
  check("def f[A : B : C]: D")      // Type.Bounds (context)
  checkType("=> T")                 // Type.ByName
  checkType("Any*")                 // Type.Repeated
  check("trait A[X]")               // Type.Param
  check("def f[@a A]: B")           // Type.Param (annotations)
  checkPat("List[t](xs @ _*)")      // Type.Var

  // meta.Term
  check("this")                       // Term.This
  check("a.this")                     // Term.This
  check("a.super.b")                  // Term.Super
  check("super[a].b")                 // Term.Super
  check("a.super[b].c")               // Term.Super
  check("a")                          // Term.Name
  check("a.b")                        // Term.Select
  check("-a")                         // Term.ApplyUnary
  check("+a")                         // Term.ApplyUnary
  check("~a")                         // Term.ApplyUnary
  check("!a")                         // Term.ApplyUnary
  check("a(b)")                       // Term.Apply
  check("a { case x => x }")          // Term.Apply
  check("a { b }")                    // Term.Apply
  check("a[B]")                       // Term.ApplyType
  check("a + a")                      // Term.ApplyInfix
  checkStructural("a + (())")         // Term.ApplyInfix
  check("(a, b) + c")                 // Term.ApplyInfix
  check("a + (b, c)")                 // Term.ApplyInfix
  check("a + ((b, c))")               // Term.ApplyInfix
  check("a = 1")                      // Term.Assign
  check("return b")                   // Term.Return
  check("throw e")                    // Term.Throw
  check("a: A")                       // Term.Ascribe
  check("x: @u")                      // Term.Annotate
  check("(1, 1)")                     // Term.Tuple
  checkStructural("{ a; b }")         // Term.Block
  check("if (p) t ")                  // Term.If
  check("if (p) {}")                  // Term.If
  check("if (p) if (p2) t")           // Term.If
  check("if (p) t else f")            // Term.If
  check("x match { case _ => }")      // Term.Match
  check("try f finally {}")           // Term.Try
  check("try f catch { case _ => }")  // Term.Try
  check("try f catch h")              // Term.TryWithHandler
  check("try f catch h finally {}")   // Term.TryWithHandler
  check("(a, b) => a + b")            // Term.Function
  check("{ case _ => }")              // Term.PartialFunction
  check("while (p) d")                // Term.While
  check("do d while (p)")             // Term.Do
  check("for { x <- xs } f(x)")       // Term.For
  check("for { x <- xs } yield f(x)") // Term.ForYield
  check("(new A)")                    // Term.New
  check("new A(1)")                   // Term.New
  check("new A {}")                   // Term.NewAnonymous
  check("_")                          // Term.Placeholder
  check("f _")                        // Term.Eta
  check("f(x: _*)")                   // Term.Repeated
  check("def f(x: A): B")             // Term.Param
  check("def f(x: A = 1): B")         // Term.Param (default parameter)
  check("s\"a $b\"")                  // Term.Interpolate
  check("<a>b {c}</a>")               // Term.Xml

  // meta.Pkg
  checkSource("package a")
  checkSourceStructural(
    """|package a.b {
     |  package c {
     |
     |  }
     |}""".stripMargin
  )
  checkSourceStructural(
    """|package a {
     |  class A
     |}
     |class B""".stripMargin
  )
  checkTreeSource(Pkg(Term.Name("a"), List())) // no parent
  checkSource("package object a") // Pkg.Object

  // meta.{Import, Importee, Importer}
  check("import a.b")
  check("import a.b, c.d")
  check("import a._, c._")
  check("import a._")
  check("import a.{ b, c }")
  check("import a.{ b => c }")
  check("import a.{ b => _ }")

  // meta.Self
  check("trait A { self: B => }")
  check("trait A { _: B => }")
  check("trait A { self => }")
  checkStructural("trait A { this: B => }")

  // meta.Template
  check("new A {}")
  check("class A extends B with C with D")
  check("class Y extends { val a = 1 } with X")
  check("new { val a = 1 } with A {}")

  // meta.Member
  
  // meta.Decl
  check("val a: Int")      // Decl.Val
  check("var b: Long")     // Decl.Var
  check("def f: String")   // Decl.Def
  check("type T")          // Decl.Type

  // meta.Defn
  check("val a = 1")              // Defn.Val
  check("var a = 1")              // Defn.Var
  check("var a: Int = _")         // Defn.Var
  check("def a = 1")              // Defn.Def
  check("def f = macro m")        // Defn.Macro
  check("type T = Int")           // Defn.Type
  check("class A")                // Defn.Class
  check("class A(b: B)")          // Defn.Class
  check("class A private (b: B)") // Defn.Class
  check("trait A")                // Defn.Trait
  check("object A")               // Defn.Object

  // meta.Ctor
  check("class A { def this(a: A) = this() }") // Ctor.Secondary
  check(
    """|class A {
     |  def this(a: A) = {
     |    this()
     |
     |    f(b)
     |  }
     |}""".stripMargin
  ) // Ctor.Secondary

  // meta.Mod
  check("@tailrec def f = 1")       // Mod.Annotation
  check("private[foo] val a = 1")   // Mod.Private
  check("protected[foo] val a = 1") // Mod.Protected
  check("implicit val a = 1")       // Mod.Implicit
  check("final val a = 1")          // Mod.Final
  check("sealed trait a")           // Mod.Sealed
  check("override def f = 1")       // Mod.Override
  check("case object B")            // Mod.Case
  check("abstract class A")         // Mod.Abstract
  check("class A[+ T]")             // Mod.Covariant
  check("class A[- T]")             // Mod.Contravariant
  check("lazy val a = 1")           // Mod.Lazy
  check("class A(val b: B)")        // Mod.ValParam
  check("class A(var b: B)")        // Mod.VarParam
  check("inline def f = 1", dotty)  // Mod.Inline

  // meta.Pat
  checkCase("case `1` =>")              // Lit
  checkPat("a")                         // Term.Name
  checkPat("a.b")                       // Term.Select
  checkPat("a @ A")                     // Pat.Var, Pat.Bind
  checkPat("_")                         // Pat.Wildcard
  checkPat("_*")                        // Pat.SeqWildcard
  checkPat("a | b")                     // Pat.Alternative
  checkPat("(a, b)")                    // Pat.Tuple
  checkPat("E(a, b)")                   // Pat.Extract
  checkPat("a E b")                     // Pat.ExtractInfix
  checkPat("""r"example (.+)${foo}"""") // Pat.Interpolate
  checkPat("<h1>a{b}c{d}e{f}g</h1>")    // Pat.Xml
  checkPat("foo: Int")                  // Pat.Typed
  checkPat("foo: Int")                  // Pat.Typed
}
