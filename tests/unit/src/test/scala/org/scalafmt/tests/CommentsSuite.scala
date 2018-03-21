// format: off
package org.scalafmt.tests

object CommentsSuite extends BaseScalaPrinterTest {

  // meta.Term
  check("/* L */ f /* I */() // T")    // Term.Apply
  check("/* L */ a op f // T")         // Term.ApplyInfix
  check("/* L */ (1, /* I */ 1) // T") // Term.Tuple

  // meta.Decl
  check("/* L */ val a: Int // T")    // Decl.Val
  check("/* L */ var b: Long // T")   // Decl.Var
  check("/* L */ def f: String // T") // Decl.Def
  check("/* L */ type S // T")        // Decl.Type

  // meta.Defn
  check("/* L */ val a = 1")              // Defn.Val
  check("/* L */ var a = 1")              // Defn.Var
  check("/* L */ var a: Int = _")         // Defn.Var
  check("/* L */ def a = 1")              // Defn.Def
  check("/* L */ def f = macro m")        // Defn.Macro
  check("/* L */ type S = Int")           // Defn.Type
  check("/* L */ class A")                // Defn.Class
  check("/* L */ class A(b: B)")          // Defn.Class
  check("/* L */ class A private (b: B)") // Defn.Class
  check("/* L */ trait A")                // Defn.Trait
  check("/* L */ object A")               // Defn.Object

  // Top level
  checkSource("/* L */ package object a") // Pkg.Object
  checkSource("/* L */ package A")        // Pkg
  checkSource("/* L */ import a.b")       // Import

  // meta.Mod
  // TODO: trailing
  check("/* L */ @tailrec def f = 1")       // Mod.Annotation
  check("/* L */ private[foo] val a = 1")   // Mod.Private
  check("/* L */ protected[foo] val a = 1") // Mod.Protected
  check("/* L */ implicit val a = 1")       // Mod.Implicit
  check("/* L */ final val a = 1")          // Mod.Final
  check("/* L */ sealed trait a")           // Mod.Sealed
  check("/* L */ override def f = 1")       // Mod.Override
  check("/* L */ case object B")            // Mod.Case
  check("/* L */ abstract class A")         // Mod.Abstract
  check("/* L */ lazy val a = 1")           // Mod.Lazy
  check("/* L */ inline def f = 1", dotty)  // Mod.Inline
  // TODO check("class A[/* L */+ T]")      // Mod.Covariant
  // TODO check("class A[/* L */- T]")      // Mod.Contravariant
  // TODO check("class A(/* L */val b: B)") // Mod.ValParam
  // TODO check("class A(/* L */var b: B)") // Mod.VarParam


  // == Advanced ==

  // Term.ApplyInfix
  check(
    """|{
       |  a & // T
       |   b
       |}"""
  )
  // Term.ApplyInfix
  check(
    """|(b // T
       | & c)"""
  )
}
