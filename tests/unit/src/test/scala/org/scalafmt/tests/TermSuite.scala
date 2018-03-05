package org.scalafmt.tests

object TermSuite extends BaseScalaPrinterTest {
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
  check("def a[@b B](c: C) = c")
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
  check("""{
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
  check("a(b: _*)")
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
  check("""a((throw b): B)""")
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

  check("-(a + b)")
  check(
    """def this(a: A) = {
      |  this()
      |
      |  this.a = a
      |}
    """.stripMargin
  )
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
  check("""a must {
          |  b
          |
          |  c
          |}""".stripMargin)
  check("""
          |a {
          |  b
          |
          |  c
          |}
        """.stripMargin)
  check("""
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
  check("while (a) b")
  check("do a while (b)")
  check(
    "def aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa(): Unit"
  )
  check("""
          |def a = {
          |  case A =>
          |    b
          |
          |  case A =>
          |    b
          |}
        """.stripMargin)
  check("new {}")
}
