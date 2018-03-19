package org.scalafmt.tests

object CommentsSuite extends BaseScalaPrinterTest {
  check(
    """|/* L1
       | * L2
       | */
       |class A // T""".stripMargin
  )
  check("class A // T")
  check(
    """|{
       |  a & // T
       |   b
       |}""".stripMargin
  )
  check("{ A/* T */(1) }")
  check(
    """|(b // T
       | & c)""".stripMargin
  )
  checkSource(
    """|// L
       |
       |package A""".stripMargin
  )
  checkSource(
    """|// L
       |package A""".stripMargin
  )
  check("/* C */ implicit class A")

  checkSource("/* L */ import a.b")
  checkSource("import a.b // T")

  check("/* L */ trait A")

  check("f() // T")
  check("/* L */ f()")

  check("a op f // T")

  check("/* L */ @a.b def f: Unit")
  check("""|@a.b // T
           | def f: Unit""".stripMargin)

  check("/* L */ private[z] class A")
  // check("private[z] /* T */ class A")
  check("/* L */ protected[z] class A")
  // check("protected[z] /* T */ class A")
}
