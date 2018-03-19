package org.scalafmt.tests

object CommentsSuite extends BaseScalaPrinterTest {
  check(
    """|/* java
       | * doc
       | */
       |class A // trailing""".stripMargin
  )
  check("class A // trailing")
  check(
    """|{
       |  a & // trailing
       |   b
       |}""".stripMargin
  )
  check("{ A/*C*/(1) }")
  check(
    """|(b // c
       | & c)""".stripMargin
  )
  checkSource(
    """|// test
       |
       |package A""".stripMargin
  )
  checkSource(
    """|// c1
       |package A""".stripMargin
  )
  check("/* C */ implicit class A")
  checkSource("/* C */ import a.b")
}
