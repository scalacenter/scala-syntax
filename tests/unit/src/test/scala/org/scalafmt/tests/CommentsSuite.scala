package org.scalafmt.tests

object CommentsSuite extends BaseScalaPrinterTest {
  check(
    """|{
       |  /* java
       |   * doc
       |   */
       |  val a = 1
       |  class A   // trailing
       |}""".stripMargin
  )
}
