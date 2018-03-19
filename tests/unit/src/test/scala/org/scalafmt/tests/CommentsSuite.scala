package org.scalafmt.tests

object CommentsSuite extends BaseScalaPrinterTest {
  check(
    """|/* java
       | * doc
       | */
       |class A // trailing""".stripMargin
  )
  check("class A // trailing")
}
