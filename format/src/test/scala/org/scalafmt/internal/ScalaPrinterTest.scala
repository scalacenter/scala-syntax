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
  check("def a[A](b: B = c) = 1")
  check("def a(implicit b: B, c: C) = b")
  check("val a: Int = 1")
  check("var a: Int = 1")
  check("def a: Int = 1")
  check("def a: Int")
  check("type A = B")
  check("type A >: B <: C")
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
  check("class A[B] extends C with D { val a = 1 }")
  check("object A extends B with C { val x = 1 }")
  check("trait a { self: D => }")
  check("trait A { _: A => }")
  check("class A extends { var x = 2 } with B")
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
  checkType("A B C")
  checkType("A => C")
  checkType("(B => C) => (A => B)")
  check("def a(b: ((A, B)) => C) = b")
  checkType("(A, B) => C")
  checkType("() => C")
  checkType("implicit A => C")
  checkType("A & B")
  checkType("A with B")
  checkType("A { val b: B }")
  checkType("{ val b: B }")
  checkType("A[T] forSome { type T }")
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
  checkPat("A(`a`)")
  checkCase("case `a` =>")
  checkCase("case `a` | `b` =>")
  checkCase("case a @ `b` =>")
  checkPat("a @ (1 | 2)")
  checkEnumerator("`a` <- b")
  checkEnumerator("a <- b")
  checkEnumerator("a = b")
  checkCase("case `a` :: `b` :: _ =>")
  checkCase(""" case a b `c` =>""")

  // term
  check("'c'")
  check("(a + b).c")
  check("(if (a) b else c).d")
  check("(if (a) b else c) + d")
  check("def foo(): Unit = {}")
  check(
    """
      |{
      |  a
      |
      |  b
      |}
    """.stripMargin
  )
  check("""{
          |  def a = {}
          |
          |  when()
          |}
    """.stripMargin)
  check("def a: A = macro b")
  check("""0 /: tags((sum, t) => sum + getInt(m, t))""")
  checkSource("""package a {
                |  class A
                |}
                |
                |package b {
                |  class B
                |}
    """.stripMargin)
  check("""{
          |  <xml>
          |    <p></p>
          |  </xml>
          |}""".stripMargin)
  check("""|val a =
           |  <a>
           |    <b>{b}</b>
           |  </a>""".stripMargin)
  check(
    """{
      |  val x = <a></a>
      |
      |  (<b></b>)
      |}
    """.stripMargin)
  check("(a + b) { c => d }")
  check("(a + b) { case c => d }")
  check("return a")
  check("(a, b)")
  check("a(1)")
  check("a[A]")
  check("a(1, 2)")
  check("a(1, 2)(1)")
  check("a(b:_*)")
  check("a + b")
  check("a op[T] b")
  check("a(b => c)")
  check("a((b: B) => c)")
  check("a(() => b)")
  check("a { case 1 => }")
    check("forAll { (f: Long => Long) => a }")
  check(
    """a {
      |  case 1 =>
      |
      |  case 2 =>
      |}""".stripMargin
  )
  check("""if (true) 1 else 2""".stripMargin)
  check("for { a <- b } a")
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
  check("try a finally b()")
  check("try a catch { case _: A => } finally c")
  check("a match { case 1 => }")
  check("""(a: @switch) match { case _ => b }""".stripMargin)
  check("""a((throw b): B)""")
  check("(if (a) b else c) match { case 1 => }")
  check("this")
  check("a((a ++ b)[A] == c)")
  check("super[A].a")
  check("super.a")
  check("this a b[B]")
  check("a.super[A].a")
  check("new A {}")
  check("new (A ~> B) {}")
  check("new { val a = 1 } with B { def b = 1 }")
  check("new A with B")
  check("new A with B { def a = 1 }")
  check(""" q"b" """)
  check(""" q"b\n" """)
  check(" q\"\"\"b\n\"\"\" ")
  check(" q\"b$b\" ")
  check(" q\"b$b$c\" ")
  check(" q\"b$b_$c\" ")
  check(" q\"b${b.c}\" ")
  check(" q\"b${1 + q\"a\"}\" ")
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
  check("-(a + b)")
  check(
    """def this(a: A) = {
      |  this()
      |
      |  this.a = a
      |}
    """.stripMargin
  )
  check(""" s"$$a" """.stripMargin)
  check("a")
  check("a a { b => c => d }")
  check("a { b => implicit c => d }")
  check("a { implicit b => implicit c => d }")
  check("""a("\\n")""")
  check("""a("[\" \\u00e4\\u00e4li\\u00f6t\"]") """.stripMargin)
  check("""
          |a(
          |  b,
          |  { implicit c =>
          |    d
          |
          |    e
          |  }
          |)
        """.stripMargin)
  check(
    """
      |a {
      |  b
      |
      |  c
      |}
    """.stripMargin)
  check(
    """
      |if (a)
      |  b
      |else if (c)
      |  d
      |else {
      |  a
      |
      |  b
      |}
    """.stripMargin)
  check("(new A).a")
  check("new A().a")
  check("a(_ => b)")
  check("a { _: B => b }")
  check("a { b: B => b }")

  // infix precedence/associativity
  check("(a :!= b) == c")
  check("b -> (c :: d)")
  check("a + (())")
  check("a :: b :: c")
  check("(a :: b) :: c")
  check("a + b * c")
  check("a + b - c")
  check("a + (b - c) * d")
  check("a + (b - c * d)")
  check("(a + b) * c")
  check("(a + b) * c * d")
  check("(a + b) * (c * d)")
  check("(a + b) * (if (true) c else d)")
  check("b -> (c + d)")
  check("(a :: b) == c d ==(e)")
  checkCase("case (a :: b) :: c =>")
  checkCase("case (a @ A()) :: c =>")
  checkType("(A :: B) :: C")
  checkType("(A :: b.B) :: C")
  check("(a /: b) to 5")
  check("r /: (1 to 5)")
  check("a(b :: (c d e):_*)")
  check("(a := b) op c")
  check("a t_= (a b c)")
  check("(a b c) +: d")
}
