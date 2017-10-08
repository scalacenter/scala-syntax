package org.scalafmt.tests

import scala.meta.dialects
import org.scalafmt.Options
import org.scalafmt.internal.BaseScalaPrinterTest

class ScalaPrinterTest extends BaseScalaPrinterTest {

  check("package a.b", Options.default)
  check("import a.b")
  check("import a.b, c.d")
  check("import a._")
  check("import a.{ b, c }")
  check("import a.{ b => c }")
  check("import a.{ b => _ }")
  check("val a = 1")
  check("var a = 1")
  check("def a = 1")
  check("def a(b: B) = 1")
  check("def a[A](b: B) = 1")
  check("val a: Int = 1")
  check("var a: Int = 1")
  check("def a: Int = 1")
  check("def a: Int")
  check("type A = B")
  check("type A")
  check("type A[B] = C")
  check("val a: Int")
  check("var a: Int")
  check("var a, b = 1")
  check("val a, b = 1")
  check("var a, b: Int")
  check("val a, b: Int")
  check("val a_ : Int")
  check("class A")
  check("object A")
  check("trait A")
  check("class A private (a: A)")
  check("final case class A()")
  check("abstract class A(val b: B)")
  check("implicit class A(var b: B)")
  check("private class A")
  check("private[a] class A")
  check("private[this] class A")
  check("protected[a] def b = 2")
  check("protected def a = 2")
  check("sealed trait A")
  check("@a def b = 1")
  check("@a(1) def b = 1")
  check("@a @b def c = 1")
  check("def a(@b c: C) = 1")
  check("def a(implicit b: B) = 1")
  check("class A[B] extends C with D {\n  val a = 1\n}")
  check("object A extends B with C {\n  val x = 1\n}")
  check("trait a {\n  self: D =>\n}")
  check("class A extends {\n  var x = 2\n} with B")
  check("class A extends B[C](1)")
  check("def this(a: A) = this(a)")
  check("def a[A: B]: Unit")
  check("def a[A : B : C]: Unit")
  check("def a[A <% B]: Unit")

  // type
  checkType("(A, A)")
  checkType("b.B")
  checkType("A#B")
  checkType("b.type")
  checkType("this.type")
  checkType("(A B C)")
  checkType("A => C")
  checkType("(A, B) => C")
  checkType("() => C")
  checkType("implicit A => C")
  checkType("(A & B)")
  checkType("(A with B)")
  checkType("""A {
              |  val b: B
              |}""".stripMargin)
  checkType("""{
              |  val b: B
              |}""".stripMargin)
  checkType("""A[T] forSome {
              |  type T
              |}""".stripMargin)
  checkType("A @a")
  checkType("A[[B] => B]")
  checkType("A[_]")
  checkType("A[_ <: B]")
  check("def a(b: B*): B")
  check("def empty[T <: AnyRef, K[_ <: T]]: TypedMultiMap[T, K]")

  // pat
  checkPat("1 | 2")
  checkPat("(c, d)")
  checkPat("a op b")
  checkPat("a op (b, c)")
  checkPat("(a, b) op (c, d)")
  checkPat(""" q"a" """)
  checkPat("a @ A")
  checkPat("A()")
  checkPat("A(a)")
  checkPat("A(a, b)")

  // term
  check("'c'")
  check(
    """
      |{
      |  a
      |
      |  b
      |}
    """.stripMargin
  )
  check("return a")
  check("(a, b)")
  check("a(1)")
  check("a[A]")
  check("a(1, 2)")
  check("a(1, 2)(1)")
  check("a(b:_*)")
  check("(a + (b))")
  check("(a op[T] (b))")
  check("a(b => c)")
  check("""a {
          |  case 1 =>
          |}""".stripMargin)
  check(
    """a {
      |  case 1 =>
      |
      |  case 2 =>
      |}""".stripMargin
  )
  check("""if (true)
          |  1
          |else
          |  2""".stripMargin)
  check("""for {
          |  a <- b
          |} a""".stripMargin)
  check(
    """for {
      |  a <- b
      |
      |  _ = a
      |
      |  if c
      |} yield a""".stripMargin
  )
  check("try a catch b")
  check("try a catch { case _: A => } finally c")
  check("""a match {
          |  case 1 =>
          |}""".stripMargin)
  check("this")
  check("super[A].a")
  check("super.a")
  check("a.super[A].a")
  check(""" q"b" """)
  check(" q\"b\\n\" ")
  check(" q\"\"\"b\n\"\"\" ")
  check(" q\"b$b\" ")
  check(" q\"b$b$c\" ")
  check(" q\"b$b_$c\" ")
  check(" q\"b${b.c}\" ")
  check(" q\"b${(1 + (q\"a\"))}\" ")
  check(" q\"b${b}_\" ")
  check(
    """
      |foo('''
      |''')
      |""".stripMargin,
    """
      |foo(
      |  '''
      |'''
      |)""".stripMargin
  )

  check(
    """
      |foo(s'''
      |''')
      |""".stripMargin,
    """
      |foo(
      |  s'''
      |'''
      |)""".stripMargin
  )

  check(
    """
      |foo(s'''
      | $s
      |''')
      |""".stripMargin,
    """
      |foo(
      |  s'''
      | $s
      |'''
      |)""".stripMargin
  )
  check(""" s"$$a" """.stripMargin)
}
