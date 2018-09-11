package scala.meta.internal.prettyprinters

import scala.meta._

object AssociatedTriviasSuite extends DiffSuite {

  def check(source: String, expected: String): Unit = {
    val in = source.stripMargin
    val trivias = AssociatedTrivias(in.parse[Stat].get)
    val obtained = trivias.toString

    assertNoDiff(obtained, expected.stripMargin)
  }

  test("ok") {
    check(
      """|trait A {
         |  /**
         |    * L
         |    */
         |  def foo: Int
         |}""",
      """|AssociatedTrivias(
         |  Leading =
         |    def [33..36) => [/**¶∙∙∙∙*∙L¶∙∙∙∙*/,¶]
         |  Trailing =
         |
         |)""".stripMargin
    )
  }

  test("basic") {
    check(
      """|{
         |  /* java
         |   * doc
         |   */
         |  val a = 1
         |  class A   // trailing
         |}""",
      """|AssociatedTrivias(
         |  Leading =
         |    val [29..32) => [/*∙java¶∙∙∙*∙doc¶∙∙∙*/,¶]
         |  Trailing =
         |    A [47..48) => [//∙trailing,¶]
         |)"""
    )
  }

  test("separated leading") {
    check(
      """|// test
         |
         |class A""",
      """|AssociatedTrivias(
         |  Leading =
         |    class [9..14) => [//∙test,¶,¶]
         |  Trailing =
         |
         |)""",
    )
  }

  test("single line javadocs") {
    check(
      """|// c1
         |// c2
         |class A""",
      """|AssociatedTrivias(
         |  Leading =
         |    class [12..17) => [//∙c1,¶,//∙c2,¶]
         |  Trailing =
         |
         |)""",
    )
  }

  test("trailing EOF") {
    check(
      "class A // trailing",
      """|AssociatedTrivias(
         |  Leading =
         |
         |  Trailing =
         |    A [6..7) => [//∙trailing]
         |)"""
    )
  }
  test("trailing NL EOF") {
    check(
      """|class A // trailing
         |""",
      """|AssociatedTrivias(
         |  Leading =
         |
         |  Trailing =
         |    A [6..7) => [//∙trailing,¶]
         |)"""
    )
  }

  test("leading EOF") {
    check(
      """|class A
         |// L
         |""",
      """|AssociatedTrivias(
         |  Leading =
         |    EOF [13..13) => [//∙L,¶]
         |  Trailing =
         |
         |)"""
    )
  }

  test("leading vs trailing 1") {
    check(
      """|class A {
         |  val a = /*L*/ b /*T*/
         |}""",
      """|AssociatedTrivias(
         |  Leading =
         |    b [26..27) => [/*L*/]
         |  Trailing =
         |    b [26..27) => [/*T*/,¶]
         |)""".stripMargin
    )
  }

  test("leading vs trailing 2") {
    check(
      """|class A {
         |  val a = // T
         |    1
         |}""",
      """|AssociatedTrivias(
         |  Leading =
         |
         |  Trailing =
         |    = [18..19) => [//∙T,¶]
         |)""".stripMargin
    )
  }

  test("attach to ident") {
    check(
      "class A /* T */ extends B",
      """|AssociatedTrivias(
         |  Leading =
         |
         |  Trailing =
         |    A [6..7) => [/*∙T∙*/]
         |)"""
    )
  }
}
